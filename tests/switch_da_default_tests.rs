use tolc::parser::parse_and_verify;

fn ok(src: &str) { let _ = parse_and_verify(src).expect("expected ok"); }
fn err_contains(src: &str, needle: &str) { let e = parse_and_verify(src).unwrap_err().to_string(); assert!(e.contains(needle), "{e}"); }

// default ensures DA: all paths assign before use
#[test]
fn switch_with_default_establishes_da() {
    let src = r#"
package p;
class T {
  void t(int x){
    int v;
    switch(x){
      case 0: v = 1; break;
      default: v = 2; break;
    }
    int y = v; // use after switch must be DA due to default
  }
}
"#;
    ok(src);
}

// no default: there exists a path where no case executes → v not DA after switch
#[test]
fn switch_without_default_does_not_establish_da() {
    let src = r#"
package p;
class T {
  void t(int x){
    int v;
    switch(x){
      case 0: v = 1; break;
      case 1: v = 2; break;
    }
    int y = v; // error: may be unassigned (x != 0 && x != 1)
  }
}
"#;
    err_contains(src, "use of local variable 'v' before definite assignment");
}


