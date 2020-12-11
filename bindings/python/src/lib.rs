use pyo3::prelude::*;

#[pymodule]
fn pynlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;

    Ok(())
}
