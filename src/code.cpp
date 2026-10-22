


# include <RcppArmadillo.h>
using namespace Rcpp;

# include <cmath>
#include <cstddef>
#include <limits>
#include <vector>
namespace {


struct TerminalRegion
{
  double value;
  double lower;
  double upper;
};


  /*
   * Recursively traverses a tree stored in depth-first preorder.
   *
   * Parameters
   * ----------
   * tree_mat
   *   Tree matrix. The expected columns are:
   *
   *   column 3: split variable
   *             -1 = terminal node
   *              1 = latent-lag predictor
   *             >1 = conditioning/exogenous predictor
   *
   *   column 4: terminal value or split point
   *   column 5: latent-lag lower bound
   *   column 6: latent-lag upper bound
   *
   * start_row
   *   Zero-based row index of the root of the current subtree.
   *
   * go_bool
   *   TRUE when the current subtree is compatible with the supplied
   *   conditioning predictors.
   *
   * x_mat
   *   A one-row predictor matrix. Predictor one is the uncertain
   *   latent lag. Predictors greater than one are conditioned upon.
   *
   * terminal_regions
   *   Output container. A row is appended whenever a compatible
   *   terminal node is reached.
   *
   * Returns
   * -------
   * Number of rows/nodes in the current subtree.
   */
  std::size_t
  getPredictionsForTreeRecursive_impl(
    const arma::mat& tree_mat,
    const std::size_t start_row,
    const bool go_bool,
    const arma::mat& x_mat,
    std::vector<TerminalRegion>& terminal_regions)
  {
    if (start_row >= tree_mat.n_rows) {
      Rcpp::stop("Tree traversal reached a row outside tree_mat.");
    }

    const double split_variable_raw = tree_mat(start_row, 3);

    if (!std::isfinite(split_variable_raw)) {
      Rcpp::stop("Tree contains a non-finite split-variable value.");
    }

    const int split_variable = static_cast<int>(split_variable_raw);
    /* Terminal node. */
    if (split_variable == -1) {
      if (go_bool) {
        const double terminal_value = tree_mat(start_row, 4);
        const double lower_bound = tree_mat(start_row, 5);
        const double upper_bound = tree_mat(start_row, 6);
        if (!std::isfinite(terminal_value) || std::isnan(lower_bound) || std::isnan(upper_bound)) {
          Rcpp::stop("Compatible terminal node contains invalid values.");
        }
        if (lower_bound >= upper_bound) {Rcpp::stop("Compatible terminal node has lower >= upper.");}
        TerminalRegion region;
        region.value = terminal_value;
        region.lower = lower_bound;
        region.upper = upper_bound;
        terminal_regions.push_back(region);
      }
      // A terminal subtree consists of one node.
      return 1U;
    }

    if (split_variable < 1) {
      Rcpp::stop("Nonterminal node has an invalid split-variable index.");
    }

    /* * The left child immediately follows the current node in   * preorder storage. */
    const std::size_t left_start = start_row + 1U;

    if (left_start >= tree_mat.n_rows) {
      Rcpp::stop("Nonterminal node has no left child.");
    }

    /* * Split on predictor one: the uncertain latent lag. *
     * * Do not condition on its current numerical value. Traverse both
     * branches so that all intervals of the lag dimension are retained.
     */
    if (split_variable == 1) {
      const std::size_t num_nodes_left =
        getPredictionsForTreeRecursive_impl(
          tree_mat,
          left_start,
          go_bool,
          x_mat,
          terminal_regions
        );

      const std::size_t right_start = left_start + num_nodes_left;
      if (right_start >= tree_mat.n_rows) {
        Rcpp::stop("Lag-splitting node has no valid right child.");
      }

      const std::size_t num_nodes_right =
        getPredictionsForTreeRecursive_impl(
          tree_mat,
          right_start,
          go_bool,
          x_mat,
          terminal_regions
        );

      return 1U + num_nodes_left + num_nodes_right;
    }


    /*
     * Split on a conditioning predictor. *
     * Extracted dbarts split variables are assumed to be one-based.
     * Hence split_variable - 1 is the zero-based x_mat column.
     */
    const std::size_t predictor_column = static_cast<std::size_t>( split_variable - 1 );

    if (predictor_column >= x_mat.n_cols) {
      Rcpp::stop( "Tree split-variable index exceeds the number of predictors." );
    }

    const double predictor_value = x_mat(0, predictor_column);

    const double split_point = tree_mat(start_row, 4);

    if (!std::isfinite(predictor_value) || !std::isfinite(split_point) ) {
      Rcpp::stop( "Non-finite predictor value or split point." );
    }

    const bool goes_left = predictor_value <= split_point;


    /* * We must still traverse both subtrees to determine their sizes. *
     * Only the branch compatible with the conditioning predictor keeps
     * go_bool equal to TRUE. Terminal nodes in the incompatible branch
     * are therefore not appended to terminal_regions.
     */
    const std::size_t num_nodes_left =
      getPredictionsForTreeRecursive_impl(
        tree_mat,
        left_start,
        go_bool &&
          goes_left,
          x_mat,
          terminal_regions
      );

    const std::size_t right_start = left_start + num_nodes_left;

    if (right_start >= tree_mat.n_rows) {
      Rcpp::stop( "Conditioning split has no valid right child." );
    }

    const std::size_t num_nodes_right =
      getPredictionsForTreeRecursive_impl(
        tree_mat,
        right_start,
        go_bool &&
          !goes_left,
          x_mat,
          terminal_regions
      );

    return 1U + num_nodes_left + num_nodes_right;
  }
} // anonymous namespace


// [[Rcpp::export]]
arma::mat getPredictionsRangesForTreeMCP_cpp( const arma::mat& tree_mat, const arma::mat& x_mat)
{
  if (tree_mat.n_rows == 0U) { Rcpp::stop( "tree_mat must contain at least one node." ); }
  if (tree_mat.n_cols < 7U) { Rcpp::stop( "tree_mat must contain at least seven columns." ); }
  if (x_mat.n_rows != 1U) { Rcpp::stop( "x_mat must contain exactly one predictor row." ); }
  if (x_mat.n_cols == 0U) { Rcpp::stop( "x_mat must contain at least the latent-lag predictor." ); }

  std::vector<TerminalRegion> terminal_regions;

  /* * A binary tree with n nodes can contain at most (n + 1) / 2
   * terminal nodes. Reserving capacity avoids repeated reallocations.
   */
  terminal_regions.reserve( ( tree_mat.n_rows + 1U ) / 2U );

  const std::size_t num_nodes_visited =
    getPredictionsForTreeRecursive_impl(
      tree_mat,
      0U,
      true,
      x_mat,
      terminal_regions
    );


  /* * For a valid preorder representation, traversing the root subtree
   * should consume every row of the tree matrix.
   */
  if (num_nodes_visited != tree_mat.n_rows) { Rcpp::stop( "Tree traversal did not consume every row of tree_mat." ); }

  if (terminal_regions.empty()) { Rcpp::stop( "No terminal region is compatible with the predictor row." ); }
  arma::mat outputmat( terminal_regions.size(), 3U );

  for ( std::size_t index = 0U; index < terminal_regions.size(); ++index ) {
    outputmat(index, 0U) = terminal_regions[index].value;
    outputmat(index, 1U) = terminal_regions[index].lower;
    outputmat(index, 2U) = terminal_regions[index].upper;
  }


  /*
   * The depth-first traversal should normally return lag intervals in
   * left-to-right order. Sorting by lower bound makes that contract
   * explicit and protects downstream lookup code.
   */
  const arma::uvec ordering = arma::sort_index( outputmat.col(1U) );

  outputmat = outputmat.rows( ordering );


  /* * Validate the returned intervals. */
  for (arma::uword row = 0U; row < outputmat.n_rows; ++row ) {
    if ( !std::isfinite( outputmat(row, 0U) ) || std::isnan( outputmat(row, 1U) ) || std::isnan(outputmat(row, 2U))) {
      Rcpp::stop("Returned terminal-region matrix contains invalid values.");
    }
    if (outputmat(row, 1U) >= outputmat(row, 2U) ) {
      Rcpp::stop("Returned terminal region has lower >= upper.");
    }
  }


  /*
   * Conditional on the exogenous predictors, the terminal regions of
   * one tree should normally form a contiguous partition of the lag
   * axis. The tolerance allows for floating-point representation.
   */
  const double tolerance = 100.0 * std::numeric_limits<double>::epsilon();
  for ( arma::uword row = 1U;row < outputmat.n_rows; ++row ) {
    const double preceding_upper = outputmat( row - 1U,2U);
    const double current_lower = outputmat(row,1U);
    const double scale = 1.0 + std::max( std::abs(preceding_upper),std::abs(current_lower));
    if (std::isfinite(preceding_upper) && std::isfinite( current_lower) &&
        std::abs( preceding_upper - current_lower) > tolerance * scale) {
      Rcpp::stop("Returned lag regions are not contiguous.");
    }
  }


  return outputmat;
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rebuildTreeRecurse_cpp(const arma::mat& tree_mat1,
                                double lower1, double upper1,
                                int rowind,
                                arma::mat tree_mat) {


  // Rcout << "LIne 14. tree_mat1.n_rows = " << tree_mat1.n_rows << ". \n";
  //
  // Rcout << "line 13 . \n";
  //
  // Rcout << "lower1 = " << lower1 << ". \n";
  // Rcout << "upper1 = " << upper1 << ". \n";

  // create a vector with first element corresponding to rowind
  // and second element corresponding to n_nodes

  // arma::uvec nodevec = { rowind, 0};
  tree_mat(0,7) = rowind;
  tree_mat(0,8) = 0;

  // Rcout << "LIne 23. tree_mat1(0,4) = " << tree_mat1(0,3) << ". \n";


  if(tree_mat1(0,3) == -1){
    // nodevec(1) = 1;
    tree_mat(0,8) = 1;

    // NOTE rowind indexes from zero since C++
    // Must subtract 1 in initial input in R
    // Rcout << "lower1 = " << lower1 << ". \n";
    // Rcout << "upper1 = " << upper1 << ". \n";

    tree_mat(rowind,5) = lower1;
    tree_mat(rowind,6) = upper1;


    // return(nodevec);
    return(tree_mat);
  }

  double leftlower = lower1;

  // Rcout << "line 34 . \n";

  double leftupper = 0;
  if(tree_mat1(0,3) == 1){
    leftupper = tree_mat1(0,4) ;

  }else{
    leftupper = upper1;

  }

  // Rcout << "line 46 . \n";
  // Rcout << "tree_mat1.n_rows = " << tree_mat1.n_rows << ". \n";

  arma::mat headOfLeftBranch = tree_mat1.rows(1,tree_mat1.n_rows -1) ;

  // Rcout << "line 50 . \n";

  // int nextrow =  nodevec(0) + 1;
  int nextrow =  tree_mat(0,7) + 1;


  // Rcout << "line 55 . \n";


  // arma::uvec left = rebuildTreeRecurse_cpp(headOfLeftBranch, leftlower, leftupper, nextrow, tree_mat);
  // arma::mat left = rebuildTreeRecurse_cpp(headOfLeftBranch, leftlower, leftupper, nextrow, tree_mat);
  tree_mat = rebuildTreeRecurse_cpp(headOfLeftBranch, leftlower, leftupper, nextrow, tree_mat);

  // int n_nodes_left = left(1);
  int n_nodes_left = tree_mat(0,8);

  // Rcout << "line 62 . \n";

  double rightupper = upper1;

  double rightlower = 0;

  if(tree_mat1(0,3) == 1){
    rightlower = tree_mat1(0,4) ;
  }else{
    rightlower = lower1;
  }

  // original line in R
  // headOfRightBranch <- tree1[seq.int(2 + n_nodes.left, nrow(tree1)),]

  arma::mat headOfRightBranch =   tree_mat1.rows(1 + n_nodes_left,
                                                 tree_mat1.n_rows -1) ;

  int nextrow2 =  tree_mat(0,7) + 1;

  // Rcout << "line 82 . \n";

  // arma::uvec right = rebuildTreeRecurse_cpp(headOfRightBranch, rightlower, rightupper, nextrow2, tree_mat);
  // arma::mat right = rebuildTreeRecurse_cpp(headOfRightBranch, rightlower, rightupper, nextrow2, tree_mat);
  tree_mat = rebuildTreeRecurse_cpp(headOfRightBranch, rightlower, rightupper, nextrow2, tree_mat);

  // int n_nodes_right = right(1);
  int n_nodes_right = tree_mat(0,8);

  // Rcout << "line 88 . \n";


  // nodevec(1) =  1 + n_nodes_left + n_nodes_right ;
  // nodevec(0) =  right(0);
  tree_mat(0,8) =  1 + n_nodes_left + n_nodes_right ;
  // tree_mat(0,7) =  tree_mat(0,7);

  return(tree_mat);
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rebuildTree2_cpp(const arma::mat& tree_mat) {

  // Rcout << "line 86 . \n";

  // Create a copy of the tree matrix
  arma::mat tree_mat1 = tree_mat;

  // Create an empty matrix with two columns, and same number of rows as tree_mat
  // Not sure what fill to use
  // arma::mat emptycols(tree_mat.n_rows , 2, arma::fill::none);
  arma::mat emptycols(tree_mat.n_rows , 4);
  emptycols.fill(arma::datum::nan);

  // above addition of empty column is not actually necessary of define inputs correctly



  // Rcout << "line 96 . \n";

  arma::mat newtree_mat = arma::join_rows(tree_mat, emptycols);
  // arma::mat tree_mat = arma::join_rows(tree_mat, emptycols);

  double lower = -1*arma::datum::inf;
  double upper = arma::datum::inf;

  //here is where recursive function defined

  int rowind = 0;

  // Rcout << "line 108 . \n";
  // Rcout << "lower = " << lower << ". \n";
  // Rcout << "upper = " << upper << ". \n";
  // Rcout << "Line 141. newtree_mat = " << newtree_mat << ". \n";



  arma::mat result = rebuildTreeRecurse_cpp(tree_mat1, lower, upper, rowind, newtree_mat) ;
  // Rcout << "line 112 . \n";
  arma::mat result2 = result.cols(0,result.n_cols -2) ;

  return(result2);

}







// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::field<arma::mat> getPredictionsForTreeRecursive_cpp(arma::field<arma::mat>& outputF , //arma::mat tree_mat,
                                                          bool gobool,
                                                          const arma::mat& x_mat//, arma::mat outputmat
) {

  // arma::field<arma::mat> outputF(2);
  //
  // outputF(1) = tree_mat;
  // outputF(2) = outputmat;

  // Rcout << "line 187 . \n";

  arma::mat tree_mat = outputF(0);
  arma::mat outputmat = outputF(1);

  // tree_mat(0, 7) = 0;


  if(tree_mat(0,3) == -1){
    // nodevec(1) = 1;
    // tree_mat(1,7) = 1;

    // NOTE rowind indexes from zero since C++
    // Must subtract 1 in initial input in R
    // Rcout << "lower1 = " << lower1 << ". \n";
    // Rcout << "upper1 = " << upper1 << ". \n";

    // Rcout << "line 204 . \n";


    if(gobool){

      // Rcout << "line 209 . \n";

      int index = tree_mat(0, 7);

      // Rcout << "line 213 . \n";


      // Rcout << "outputmat = " << outputmat << ". \n";
      //
      // Rcout << "tree_mat = " << tree_mat << ". \n";
      //
      // Rcout << "index = " << index << ". \n";


      outputmat(index,0) =  tree_mat(0,4) ;
      outputmat(index,1) =  tree_mat(0,5) ;
      outputmat(index,2) =  tree_mat(0,6) ;


      // Rcout << "line 228 . \n";

      tree_mat(0, 7) = tree_mat(0, 7) + 1 ;
      // Rcout << "line 231 . \n";

      // tree_mat(1, 7) = 1 ;
      // Rcout << "line 234 . \n";


    }

    tree_mat(0, 8) = 1 ;

    outputF(0) = tree_mat;
    outputF(1) = outputmat;
    // return(nodevec);
    return(outputF);
  }

  if(tree_mat(0,3) == 1){
    // nodevec(1) = 1;
    // tree_mat(1,7) = 1;
    // Rcout << "line 224 . \n";

    // NOTE rowind indexes from zero since C++
    // Must subtract 1 in initial input in R
    // Rcout << "lower1 = " << lower1 << ". \n";
    // Rcout << "upper1 = " << upper1 << ". \n";


    if(gobool){
      arma::mat emptyrow(1 , 3);
      emptyrow.fill(arma::datum::nan);
      // outputmat = arma::join_rows(outputmat,emptyrow );
      outputmat = arma::join_cols(outputmat,emptyrow );
    }

    //
    // fill in head of left branch here
    arma::mat headOfLeftBranch = tree_mat.rows(1,tree_mat.n_rows -1) ;

    headOfLeftBranch(0,7) = tree_mat(0,7) ;

    arma::field<arma::mat> leftinputF(2);
    leftinputF(0) = headOfLeftBranch;
    leftinputF(1) = outputmat;


    arma::field<arma::mat> leftoutputF = getPredictionsForTreeRecursive_cpp(leftinputF,
                                                                            gobool,
                                                                            x_mat);

    // Rcout << "line 279 . \n";

    // do not edit the tree with leftoutputF, just theoutput mat
    arma::mat leftouttree = leftoutputF(0);
    // int n_nodesleft = leftouttree(1, 7);
    int n_nodesleft = leftouttree(0, 8);


    tree_mat(0, 7) = leftouttree(0, 7);
    // tree_mat(1, 7) = leftouttree(1, 7);


    outputmat = leftoutputF(1);

    //
    // Rcout << "line 293 . \n";

    // Rcout << "tree_mat = " << tree_mat << ". \n";

    arma::mat headOfRightBranch = tree_mat.rows(1 + n_nodesleft,
                                                tree_mat.n_rows -1) ;

    // Rcout << "headOfRightBranch = " << headOfRightBranch << ". \n";


    headOfRightBranch(0,7) = tree_mat(0,7) ;

    arma::field<arma::mat> rightinputF(2);
    rightinputF(0) = headOfRightBranch;
    rightinputF(1) = outputmat;

    // Rcout << "line 303 . \n";

    arma::field<arma::mat> rightoutputF = getPredictionsForTreeRecursive_cpp(rightinputF,
                                                                             gobool,
                                                                             x_mat);

    // Rcout << "line 310 . \n";

    arma::mat rightouttree = rightoutputF(0);
    // int n_nodesright = rightouttree(1, 7);
    int n_nodesright = rightouttree(0, 8);

    outputmat = rightoutputF(1);


    tree_mat(0, 7) = rightouttree(0, 7) ;
    // tree_mat(1, 7) = 1 + n_nodesleft + n_nodesright ;
    tree_mat(0, 8) = 1 + n_nodesleft + n_nodesright ;

    outputF(0) = tree_mat;
    outputF(1) = outputmat;
    // return(nodevec);
    return(outputF);
  }

  if(tree_mat(0,3) > 1){

    // Rcout << "line 338 . \n";


    int splitvar  = tree_mat(0,3);

    // Rcout << "tree_mat = " << tree_mat << ". \n";
    // Rcout << "x_mat = " << x_mat << ". \n";
    // Rcout << "splitvar = " << splitvar << ". \n";

    bool goesLeft = ( x_mat(0,splitvar-1) <= tree_mat(0,4)  );
    // Rcout << "line 338 . \n";
    // Rcout << "tree_mat = " << tree_mat << ". \n";

    arma::mat headOfLeftBranch = tree_mat.rows(1,tree_mat.n_rows -1) ;

    // Rcout << "headOfLeftBranch = " << headOfLeftBranch << ". \n";

    // Rcout << "line 342 . \n";

    headOfLeftBranch(0, 7) = tree_mat(0, 7) ;

    // Rcout << "headOfLeftBranch = " << headOfLeftBranch << ". \n";

    arma::field<arma::mat> leftinputF(2);
    leftinputF(0) = headOfLeftBranch;
    leftinputF(1) = outputmat;

    // Rcout << "line 354 . \n";

    arma::field<arma::mat> leftoutputF = getPredictionsForTreeRecursive_cpp(leftinputF,
                                                                            goesLeft & gobool,
                                                                            x_mat);

    // Rcout << "line 360 . \n";

    // do not edit the tree with leftoutputF, just theoutput mat
    arma::mat leftouttree = leftoutputF(0);
    // int n_nodesleft = leftouttree(1, 7);
    int n_nodesleft = leftouttree(0, 8);

    tree_mat(0, 7) = leftouttree(0, 7);

    outputmat = leftoutputF(1);

    // Rcout << "line 381 . \n";

    arma::mat headOfRightBranch =   tree_mat.rows(1 + n_nodesleft,
                                                  tree_mat.n_rows -1) ;

    // Rcout << "line 386 . \n";

    headOfRightBranch(0, 7) = tree_mat(0, 7) ;

    arma::field<arma::mat> rightinputF(2);
    rightinputF(0) = headOfRightBranch;
    rightinputF(1) = outputmat;

    // Rcout << "line 371 . \n";

    arma::field<arma::mat> rightoutputF = getPredictionsForTreeRecursive_cpp(rightinputF,
                                                                             (!goesLeft) & gobool,
                                                                             x_mat);

    // Rcout << "line 400 . \n";

    arma::mat rightouttree = rightoutputF(0);
    // Rcout << "line 377 . \n";

    // int n_nodesright = rightouttree(1, 7);
    int n_nodesright = rightouttree(0, 8);

    // Rcout << "line 408 . \n";

    outputmat = rightoutputF(1);

    // Rcout << "line 412 . \n";

    tree_mat(0, 7) = rightouttree(0, 7) ;
    // tree_mat(1, 7) = 1 + n_nodesleft + n_nodesright ;
    tree_mat(0, 8) = 1 + n_nodesleft + n_nodesright ;
    // tree_mat(0, 7) = 1 + n_nodesleft + n_nodesright ;
    outputF(0) = tree_mat;
    outputF(1) = outputmat;
    return(outputF);

  }

  Rcpp::stop("Invalid split-variable code in tree_mat." );


  // Rcout << "line 402 . \n";

  // tree_mat(1, 7) = 1 + n_nodes.left + n_nodes.righ ;
  // outputF(1) = tree_mat;
  // outputF(2) = outputmat;
  // return(nodevec);

  return(outputF);
}






// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat getPredictionsRangesForTree3_cpp(const arma::mat& tree_mat, const arma::mat& x_mat) {


  // Rcout << "line 422 . \n";

  arma::mat outputmat(x_mat.n_rows, 3);
  outputmat.fill(arma::datum::nan);

  // int indices = 1;

  arma::mat emptycols(tree_mat.n_rows , 2);
  emptycols.fill(arma::datum::nan);

  // if (tree_mat.n_cols != 7) {
  //   Rcpp::stop(
  //     "tree_mat must contain exactly seven columns."
  //   );
  // }
  // Rcout << "line 433 . \n";

  arma::mat tree_mat1 = arma::join_rows(tree_mat, emptycols);
  // arma::mat tree_mat1 = tree_mat;

  // tree_mat1(0,tree_mat1.n_cols-1) = 0;
  tree_mat1(0, 7) = 0;
  tree_mat1(0, 8) = 0;
  // Rcout << "tree_mat1(0, 7) = " << tree_mat1(0, 7) << ". \n";


  // add column corresponding to indices to be removed at the end

  arma::field<arma::mat> outputF(2);

  outputF(0) = tree_mat1;
  outputF(1) = outputmat;

  // Rcout << "line 453 . \n";

  // arma::mat outputmat2 = getPredictionsForTreeRecursive_cpp(tree_mat1, true, outputmat);
  arma::field<arma::mat> outputF2 = getPredictionsForTreeRecursive_cpp(outputF,
                                                                       true,
                                                                       x_mat);

  // Rcout << "line 460 . \n";

  arma::mat outputmat2 = outputF2(1);


  return(outputmat2);
}


// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::field<arma::mat> getListintermats_cpp(const arma::field<arma::mat>& rebuilt_tree_list, const arma::mat& x_mat) {
  // Rcout << "line 774 . \n";
  arma::field<arma::mat> output_list(rebuilt_tree_list.n_elem);
  for(unsigned int list_ind=0; list_ind < rebuilt_tree_list.n_elem; list_ind++){
    output_list(list_ind) = getPredictionsRangesForTreeMCP_cpp(rebuilt_tree_list(list_ind), x_mat);
  }
  return(output_list);
}




// [[Rcpp::export]]
arma::vec unique_with_specials(arma::vec x) {

  // 1. Check if any NaN values exist.
  // .is_nan() returns a uvec (vector of 0s and 1s)
  bool has_nan = x.has_nan();

  // x.is_inf() detects both +Inf and -Inf. We must check them individually.
  bool has_pos_inf = arma::any(x == arma::datum::inf);
  bool has_neg_inf = arma::any(x == -arma::datum::inf);

  // 2. Get *only* the finite numbers.
  arma::uvec finite_indices = arma::find_finite(x);
  arma::vec x_finite_only = x.elem(finite_indices);

  // 3. Get unique values from the finite-only vector.
  arma::vec u_finite = arma::unique(x_finite_only);

  // 4. Rebuild the final vector, adding special values back in order.
  //    We use a std::vector as an intermediate step, as it's
  //    easier to dynamically build than resizing arma::vec.
  std::vector<double> final_values;

  if (has_neg_inf) {
    final_values.push_back(-arma::datum::inf);
  }

  // Add all the unique finite numbers
  for(double val : u_finite) {
    final_values.push_back(val);
  }

  if (has_pos_inf) {
    final_values.push_back(arma::datum::inf);
  }

  if (has_nan) {
    final_values.push_back(arma::datum::nan);
  }

  // Convert the std::vector back to an arma::vec
  // 'true' means 'copy_aux_mem'
  arma::vec u_final = arma::vec(final_values.data(), final_values.size(), true);

  return u_final;
}




// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat interNtreesB_cpp(const arma::field<arma::mat>& inter_list) {

  arma::field<arma::vec> xlist(inter_list.n_elem);
  arma::field<arma::vec> ylist(inter_list.n_elem);

  unsigned int x_length = 0;
  for(unsigned int list_ind=0; list_ind < inter_list.n_elem; list_ind++){
    arma::vec tempvec(1 + inter_list(list_ind).n_rows);
    tempvec(0) = - arma::datum::inf;
    tempvec.subvec(1,inter_list(list_ind).n_rows) = inter_list(list_ind).col(2) ;
    xlist(list_ind) = tempvec; // {- arma::datum::inf , inter_list(list_ind).col(2)};
    ylist(list_ind) = inter_list(list_ind).col(0);
    x_length = x_length +  xlist(list_ind).n_elem;
  }
  arma::vec x_unlist(x_length);
  x_length = 0;
  if(inter_list.n_elem >1){
    for(unsigned int list_ind=0; list_ind < inter_list.n_elem; list_ind++){
      // arma::vec tempvec = xlist(list_ind);
      x_unlist.subvec(x_length, x_length +  xlist(list_ind).n_elem - 1) = xlist(list_ind) ;
      x_length = x_length +  xlist(list_ind).n_elem;
    }
  }

  // Rcout << "line 539.\n";
  arma::vec xs = unique_with_specials(x_unlist);
  arma::vec ypredvec(xs.n_elem - 1);
  // Rcout << "xs = " << xs << ".\n";

  for( unsigned int i=1; i < xs.n_elem; i++){
    // Rcout << "line 544.\n";
    double tempval = xs(i);
    ypredvec(i-1) = 0.0;
    // Rcout << "line 547.\n";
    // Rcout << "i = " << i << ".\n";
    // Rcout << "tempval = " << tempval << ".\n";
    for( arma::uword j=0; j < inter_list.n_elem; j++){
      // Rcout << "j = " << j << ".\n";
      // Rcout << "xlist(j).n_elem = " << xlist(j).n_elem << ".\n";
      // Rcout << "xlist(j) = " << xlist(j) << ".\n";
      arma::uvec tempinds = arma::find(xlist(j) < tempval);
      // Rcout << "tempinds = " << tempinds << ".\n";
      arma:: uword tempind = tempinds(tempinds.n_elem-1);
      // Rcout << "tempind = " << tempind << ".\n";
      // Rcout << "ylist(j).n_elem = " << ylist(j).n_elem << ".\n";
      ypredvec(i-1) = ypredvec(i-1) + (ylist(j))(tempind);
    }
    // Rcout << "line 556.\n";
  }

  arma::mat newmat(xs.n_elem-1,3);
  newmat.col(0) = ypredvec;
  newmat.col(1) = xs.subvec(0,xs.n_elem-2);
  newmat.col(2) = xs.subvec(1,xs.n_elem-1);

  return(newmat);
}




// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat getlistinter_create_intermat(const arma::field<arma::mat>& rebuilt_tree_list, const arma::mat& x_mat) {


  // Rcout << "line 422 . \n";
  arma::field<arma::mat> inter_list(rebuilt_tree_list.n_elem);

  for(unsigned int list_ind=0; list_ind < rebuilt_tree_list.n_elem; list_ind++){

    inter_list(list_ind) = getPredictionsRangesForTreeMCP_cpp(rebuilt_tree_list(list_ind),
                x_mat);
  }

  arma::field<arma::vec> xlist(inter_list.n_elem);
  arma::field<arma::vec> ylist(inter_list.n_elem);


  unsigned int x_length = 0;
  for(unsigned int list_ind=0; list_ind < inter_list.n_elem; list_ind++){
    arma::vec tempvec(1 + inter_list(list_ind).n_rows);
    tempvec(0) = - arma::datum::inf;
    tempvec.subvec(1,inter_list(list_ind).n_rows) = inter_list(list_ind).col(2) ;
    xlist(list_ind) = tempvec; // {- arma::datum::inf , inter_list(list_ind).col(2)};
    ylist(list_ind) = inter_list(list_ind).col(0);
    x_length = x_length +  xlist(list_ind).n_elem;
  }
  arma::vec x_unlist(x_length);
  x_length = 0;
  if(inter_list.n_elem >1){
    for(unsigned int list_ind=0; list_ind < inter_list.n_elem; list_ind++){
      // arma::vec tempvec = xlist(list_ind);
      x_unlist.subvec(x_length, x_length +  xlist(list_ind).n_elem - 1) = xlist(list_ind) ;
      x_length = x_length +  xlist(list_ind).n_elem;
    }
  }

  // Rcout << "line 539.\n";
  arma::vec xs = unique_with_specials(x_unlist);
  arma::vec ypredvec(xs.n_elem - 1);
  // Rcout << "xs = " << xs << ".\n";

  for( unsigned int i=1; i < xs.n_elem; i++){
    // Rcout << "line 544.\n";
    double tempval = xs(i);
    ypredvec(i-1) = 0.0;
    // Rcout << "line 547.\n";
    // Rcout << "i = " << i << ".\n";
    // Rcout << "tempval = " << tempval << ".\n";
    for( arma::uword j=0; j < inter_list.n_elem; j++){
      // Rcout << "j = " << j << ".\n";
      // Rcout << "xlist(j).n_elem = " << xlist(j).n_elem << ".\n";
      // Rcout << "xlist(j) = " << xlist(j) << ".\n";
      arma::uvec tempinds = arma::find(xlist(j) < tempval);
      // Rcout << "tempinds = " << tempinds << ".\n";
      arma:: uword tempind = tempinds(tempinds.n_elem-1);
      // Rcout << "tempind = " << tempind << ".\n";
      // Rcout << "ylist(j).n_elem = " << ylist(j).n_elem << ".\n";
      ypredvec(i-1) = ypredvec(i-1) + (ylist(j))(tempind);
    }
    // Rcout << "line 556.\n";
  }

  arma::mat newmat(xs.n_elem-1,3);
  newmat.col(0) = ypredvec;
  newmat.col(1) = xs.subvec(0,xs.n_elem-2);
  newmat.col(2) = xs.subvec(1,xs.n_elem-1);

  return(newmat);}

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
#include <vector>
#include <cmath>
#include <functional>

// [[Rcpp::export]]
arma::mat rebuildTreeFromChildren_cpp(
    const arma::mat& tree,
    int terminal_col,
    int node_size_col,
    int split_variable_col,
    int split_value_col,
    int mu_col,
    int child_left_col,
    int child_right_col,
    int root_row = 1,
    int lag_variable_index = 1,
    bool child_indices_one_based = true,
    double sample_number = 1.0,
    double tree_number = 1.0,
    bool require_all_rows_reachable = true)
{
  if (tree.n_rows == 0) {
    Rcpp::stop("tree must contain at least one row.");
    }
  if (tree.n_cols == 0) {
    Rcpp::stop("tree must contain at least one column.");
    }
  std::vector<int> supplied_columns = {terminal_col,node_size_col,split_variable_col,split_value_col,mu_col,child_left_col,child_right_col};
  for (std::size_t j = 0; j < supplied_columns.size(); ++j) {
    if (supplied_columns[j] < 1 || supplied_columns[j] > static_cast<int>(tree.n_cols)) {
      Rcpp::stop("A supplied column index is outside the tree matrix.");
    }
  }
  if (root_row < 1 ||root_row > static_cast<int>(tree.n_rows)) {
    Rcpp::stop("root_row is outside the tree matrix.");
  }
  if (lag_variable_index < 1) {
    Rcpp::stop("lag_variable_index must be positive.");
  }
  // Convert R column indices to zero-based C++ indices.
  const arma::uword terminal_c = static_cast<arma::uword>(terminal_col - 1);
  const arma::uword node_size_c = static_cast<arma::uword>(node_size_col - 1);
  const arma::uword split_variable_c = static_cast<arma::uword>(split_variable_col - 1);
  const arma::uword split_value_c = static_cast<arma::uword>(split_value_col - 1);
  const arma::uword mu_c = static_cast<arma::uword>(mu_col - 1);
  const arma::uword child_left_c = static_cast<arma::uword>(child_left_col - 1);
  const arma::uword child_right_c = static_cast<arma::uword>(child_right_col - 1);
  const arma::uword root = static_cast<arma::uword>(root_row - 1);
  // Each output row contains:
  // sample, tree, node_size, var, value, lower, upper.
  std::vector< std::vector<double> > output_rows;
  output_rows.reserve(tree.n_rows);
  std::vector<int> visited(tree.n_rows, 0);
  std::vector<int> active(tree.n_rows, 0);
  std::function<void(arma::uword, double, double)> visit;
  visit = [&](arma::uword node, double lower, double upper) {
    if (node >= tree.n_rows) {
      Rcpp::stop("A child index lies outside the tree matrix.");
    }
    if (active[node] == 1) {
      Rcpp::stop("A cycle was detected in the tree.");
    }
    if (visited[node] == 1) {
      Rcpp::stop("A node is reachable through more than one path.");
    }
    active[node] = 1;
    visited[node] = 1;
    double terminal_value = tree(node, terminal_c);
    double node_size = tree(node, node_size_c);
    if (!std::isfinite(terminal_value)) {
      Rcpp::stop("A node has a non-finite terminal indicator.");
    }
    if (!std::isfinite(node_size) || node_size < 0.0) {
      Rcpp::stop("A node has an invalid node size.");
    }
    bool is_terminal = terminal_value != 0.0;
    if (is_terminal) {
      double mu = tree(node, mu_c);
      if (!std::isfinite(mu)) {
        Rcpp::stop("A terminal node has a non-finite mu value.");
      }
      output_rows.push_back({sample_number,tree_number,node_size,-1.0,mu,lower,upper});
      active[node] = 0;
      return;
    }
    double split_variable_raw = tree(node, split_variable_c);
    double split_value = tree(node, split_value_c);
    double child_left_raw = tree(node, child_left_c);
    double child_right_raw = tree(node, child_right_c);
    if (!std::isfinite(split_variable_raw) || !std::isfinite(split_value) || !std::isfinite(child_left_raw) || !std::isfinite(child_right_raw)) {
        Rcpp::stop( "An internal node has non-finite split or child information." );
    }
    int split_variable = static_cast<int>(split_variable_raw);
    if (split_variable < 1) {
      Rcpp::stop( "An internal node has an invalid split-variable index." );
    }
    double left_rounded = std::round(child_left_raw);
    double right_rounded = std::round(child_right_raw);
    if (std::abs(child_left_raw - left_rounded) > 1e-10 || std::abs(child_right_raw - right_rounded) > 1e-10) {
      Rcpp::stop("Child indices must be integer-valued.");
    }
    long long left_index = static_cast<long long>(left_rounded);
    long long right_index = static_cast<long long>(right_rounded);
    if (child_indices_one_based) {
      left_index -= 1;
      right_index -= 1;
    }
    if (left_index < 0 || right_index < 0 || left_index >= static_cast<long long>(tree.n_rows) || right_index >= static_cast<long long>(tree.n_rows)) {
      Rcpp::stop("An internal node has an out-of-range child index.");
    }
    if (left_index == right_index) {
      Rcpp::stop( "An internal node has identical left and right children.");
    }
    // Record the parent before traversing either child.
    // This creates preorder output.
    output_rows.push_back({sample_number,tree_number,node_size, static_cast<double>(split_variable),split_value,lower,upper});
    double left_lower = lower;
    double left_upper = upper;
    double right_lower = lower;
    double right_upper = upper;
    // Only a split on the lag variable changes the lag bounds.
    if (split_variable == lag_variable_index) {
      left_upper = split_value;
      right_lower = split_value;
    }
    visit(static_cast<arma::uword>(left_index),left_lower,left_upper);
    visit(static_cast<arma::uword>(right_index),right_lower,right_upper);
    active[node] = 0;
  };
  visit(root,-arma::datum::inf,arma::datum::inf);
  if (require_all_rows_reachable) {
    std::vector<int> unreachable_rows;
    for (arma::uword row = 0; row < tree.n_rows; ++row) {
      if (visited[row] == 0) {
        unreachable_rows.push_back(
          static_cast<int>(row + 1)
        );
      }
    }
    if (!unreachable_rows.empty()) {
      Rcpp::stop("Some tree rows are not reachable from the specified root.");
    }
  }
  arma::mat output(output_rows.size(),7,arma::fill::none);
  for (std::size_t row = 0; row < output_rows.size(); ++row) {
    for (std::size_t col = 0; col < 7; ++col) {
      output(row, col) = output_rows[row][col];
    }
  }

  return output;
}
