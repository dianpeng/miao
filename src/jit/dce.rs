// Dead code elimination pass
//
// The basic dead code elimination pass will perform on 2 types of stuff
//
// 1) Expression level DCE, which is just PHI dce
//
// 2) Control flow DCE, which mainly deletes unneed branches, ie things like
//    if (true) ... or if (false) ...
