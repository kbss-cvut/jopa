package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.tdb.TDB;
import org.apache.jena.tdb.TDBFactory;

class TDBStorage extends Storage {

    private final String location;

    TDBStorage(DriverConfiguration configuration) {
        super(configuration);
        this.location = configuration.getStorageProperties().getPhysicalURI().toString();
    }

    @Override
    void initialize() {
        this.dataset = TDBFactory.createDataset(location);
    }

    @Override
    void writeChanges() throws JenaDriverException {
        try {
            TDB.sync(dataset);
        } catch (RuntimeException e) {
            throw new JenaDriverException("Unable to synchronize TDB storage with file system.", e);
        }
    }
}
