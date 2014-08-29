package cz.cvut.kbss.ontodriver.sesame;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.DataSource;

public class SesameDataSource implements DataSource {

	private final SesameDriver driver;
	private boolean open;

	public SesameDataSource(OntologyStorageProperties storageProperties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));

		this.driver = new SesameDriver(storageProperties, Collections.<String, String> emptyMap());
		this.open = true;
	}

	public SesameDataSource(OntologyStorageProperties storageProperties,
			Map<String, String> properties) {
		Objects.requireNonNull(storageProperties,
				ErrorUtils.constructNPXMessage("storageProperties"));
		Objects.requireNonNull(properties, ErrorUtils.constructNPXMessage("properties"));

		this.driver = new SesameDriver(storageProperties, properties);
		this.open = true;
	}

	@Override
	public void close() throws OntoDriverException {
		if (!open) {
			return;
		}
		try {
			driver.close();
		} finally {
			this.open = false;
		}
	}

	@Override
	public boolean isOpen() {
		return open;
	}

	@Override
	public Connection getConnection() throws OntoDriverException {
		if (!open) {
			throw new IllegalStateException("The data source is closed.");
		}
		return driver.acquireConnection();
	}

}
