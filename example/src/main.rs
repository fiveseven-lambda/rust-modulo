use modulo::modulo;

fn main() {
    let mut x = 33;
    let m = 1_000_000_007;
    modulo!(x += 33; m);
    println!("{}", x);
    modulo!(x -= 90; m);
    println!("{}", x);
    modulo!(x /= 2; m);
    println!("{}", x);
    modulo!(x *= 2; m);
    println!("{}", x);
}
