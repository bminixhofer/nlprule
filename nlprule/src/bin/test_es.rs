use nlprule::lang::es;

fn main() -> Result<(), nlprule::Error> {
    env_logger::init();
    es::correcter().test()
}
