

#include <Rcpp.h>
using namespace Rcpp;

// numerical tolerance
const double EPS = 1e-12;
const double TWOPI = 2 * M_PI;

// helper function: point exactly on segment?
inline bool point_on_segment(double x, double y,
                             double x1, double y1,
                             double x2, double y2) {
  
  // cross product = 0 -> collinear
  double cross = (x - x1) * (y2 - y1) - (y - y1) * (x2 - x1);
  if (std::abs(cross) > EPS) return false;
  
  // scalar product -> lies between points?
  double dot = (x - x1) * (x2 - x1) + (y - y1) * (y2 - y1);
  if (dot < -EPS) return false;
  
  double len_sq = (x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1);
  if (dot > len_sq + EPS) return false;
  
  return true;
}


// [[Rcpp::export]]
IntegerVector pip_cpp(NumericVector ptx,
                      NumericVector pty,
                      NumericVector plx,
                      NumericVector ply) {
  
  int npt = ptx.size();
  int npl = plx.size();
  
  IntegerVector out(npt);
  
  for (int ii = 0; ii < npt; ii++) {
    
    double x = ptx[ii];
    double y = pty[ii];
    double angle = 0.0;
    bool on_border = false;
    
    for (int jj = 0; jj < npl; jj++) {
      
      double x1 = plx[jj];
      double y1 = ply[jj];
      double x2 = plx[(jj + 1) % npl];
      double y2 = ply[(jj + 1) % npl];
      
      // point on the edge?
      if (point_on_segment(x, y, x1, y1, x2, y2)) {
        on_border = true;
        break;
      }
      
      // calculate angle
      double dtheta = atan2(y2 - y, x2 - x) - atan2(y1 - y, x1 - x);
      
      if (dtheta > M_PI) dtheta -= TWOPI;
      if (dtheta < -M_PI) dtheta += TWOPI;
      
      angle += dtheta;
    }
    
    // decision
    if (on_border) {
      out[ii] = 1;
    } else {
      out[ii] = (std::abs(angle) < M_PI) ? 0 : 1;
    }
  }
  
  return out;
}

