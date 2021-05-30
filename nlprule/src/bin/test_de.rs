use nlprule::lang::de;

fn main() -> Result<(), nlprule::Error> {
    env_logger::init();
    de::correcter().test()
}
