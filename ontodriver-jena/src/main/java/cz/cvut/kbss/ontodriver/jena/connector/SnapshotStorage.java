package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.jena.query.Dataset;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.system.Txn;

import java.util.Iterator;

class SnapshotStorage extends Storage {

    SnapshotStorage(DriverConfiguration configuration) {
        super(configuration);
    }

    @Override
    void initialize() {
        this.dataset = DatasetFactory.create();
    }

    void addCentralData(Dataset central) {
        Txn.executeRead(central, () -> {
            final Iterator<String> it = central.listNames();
            while (it.hasNext()) {
                final String name = it.next();
                dataset.addNamedModel(name, central.getNamedModel(name));
            }
            dataset.setDefaultModel(central.getDefaultModel());
        });
    }
}
