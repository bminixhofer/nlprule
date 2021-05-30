use nlprule::lang::en;

fn main() -> Result<(), nlprule::Error> {
    env_logger::init();
    en::correcter().test()
}
