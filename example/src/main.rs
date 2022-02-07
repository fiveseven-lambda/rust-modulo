use modulo::modulo;

fn main() {
    let mut x = 33;
    modulo!(x *= 33 mod 100);
    println!("{}", x);
    modulo!(x = x * 33 mod 100; true = false);
    println!("{}", x);
}
