
# include <RcppArmadillo.h>
# include <cmath>

using namespace Rcpp;



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat rebuildTreeRecurse_cpp(arma::mat tree_mat1,double lower1, double upper1, int rowind, arma::mat tree_mat) {


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
arma::mat rebuildTree2_cpp(arma::mat tree_mat) {

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
arma::field<arma::mat> getPredictionsForTreeRecursive_cpp(arma::field<arma::mat> outputF , //arma::mat tree_mat,
                                                          bool gobool,
                                                          arma::mat x_mat//, arma::mat outputmat
) {

  // arma::field<arma::mat> outputF(2);
  //
  // outputF(1) = tree_mat;
  // outputF(2) = outputmat;

  // Rcout << "line 187 . \n";

  arma::mat tree_mat = outputF(0);
  arma::mat outputmat = outputF(1);

  tree_mat(0, 7) = 0;


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
      outputmat = arma::join_rows(outputmat,emptyrow );

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
  }

  // Rcout << "line 402 . \n";

  // tree_mat(1, 7) = 1 + n_nodes.left + n_nodes.righ ;
  // outputF(1) = tree_mat;
  // outputF(2) = outputmat;
  // return(nodevec);

  return(outputF);
}






// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat getPredictionsRangesForTree3_cpp(arma::mat tree_mat, arma::mat x_mat) {


  // Rcout << "line 422 . \n";

  arma::mat outputmat(x_mat.n_rows, 3);
  outputmat.fill(arma::datum::nan);

  // int indices = 1;

  arma::mat emptycols(tree_mat.n_rows , 2);
  emptycols.fill(arma::datum::nan);


  // Rcout << "line 433 . \n";

  arma::mat tree_mat1 = arma::join_rows(tree_mat, emptycols);
  // arma::mat tree_mat1 = tree_mat;

  tree_mat1(0,tree_mat1.n_cols-1) = 0;

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
