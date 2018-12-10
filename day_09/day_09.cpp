#include <Rcpp.h>

// [[Rcpp::plugins(cpp11)]]

struct marble {
  marble* l;
  marble* r;
  marble* self;
  int value;
  
  marble(int i) 
  : value(i)  
  { 
    l = this;
    r = this;
    self = this;
  }
  
  void print_debug() {
    Rcpp::Rcout << "Value: " << value << "\n"
                << "this : " << this  << "\n"
                << "self : " << self  << "\n"
                << "l    : " << l     << "\n"
                << "r    : " << r     << "\n"
                                      << "\n";
  }
  
  void print(marble* cur) {
    marble* p = self;
    do {
      if (cur->self == p)
        Rcpp::Rcout << "(";
      Rcpp::Rcout << p->value;
      if (cur->self == p)
        Rcpp::Rcout << ")";
      Rcpp::Rcout << " ";
      p = p->r;
    } while (p != self);
    
    Rcpp::Rcout << "\n";
  }
};

marble* get_marble(marble* cur, int steps = 0) {
  marble* ptr = cur;
  if (steps > 0) {
    for(int i=0; i<steps; ++i) {
      ptr = ptr->r;
    }  
  } else if (steps < 0) {
    for(int i=0; i<std::abs(steps); ++i) {
      ptr = ptr->l;
    }  
  }
    
  return ptr;
}

marble* rm_marble(marble* cur) {
  marble* l = cur->l;
  marble* r = cur->r;
  
  cur->l->r = r->self;
  cur->r->l = l->self;
  
  return r->self;
}

marble* add_marble(marble* cur, marble* add) {
  add->l = cur->r;
  add->r  = cur->r->r;
  
  cur->r->r->l = add->self;
  cur->r->r = add->self;
  
  return add->self;
}


// [[Rcpp::export]]
std::vector<double> run_game_cpp(int players = 9, int last_marble = 24, bool verbose=false) {
  std::vector<std::unique_ptr<marble>> state;
  std::vector<double> score(players, 0);
  
  state.emplace_back(new marble(0));
  
  marble* cur = state[0]->self;
  for(int i = 1; i <= last_marble; ++i) {
    state.emplace_back(new marble(i));
    if (i % 23 != 0) {
      cur = add_marble(cur, state[i]->self);
    } else {
      marble* p = cur->l->l->l->l->l->l->l;
      score[(i-1) % players] += i + p->value;
      cur = p->r->self;
      rm_marble(p);
    }
    
    if (verbose) {
      Rcpp::Rcout << "[" << (i-1) % players + 1<< "] ";
      state[0]->print(cur);
    }
  }
  
  return score;
}

/*** R
max( run_game_cpp( 9, 25  ) ) # 32
max( run_game_cpp(10, 1618) ) # 8317
max( run_game_cpp(13, 7999) ) # 146373
max( run_game_cpp(17, 1104) ) # 2764
max( run_game_cpp(21, 6111) ) # 54718
max( run_game_cpp(30, 5807) ) # 37305

max( run_game_cpp(424, 71144) )
max( run_game_cpp(424, 7114400) )
*/


