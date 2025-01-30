package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import org.apache.jena.dboe.base.file.Location;
import org.apache.jena.tdb2.TDB2Factory;

class TDB2Storage extends LocalStorage {

    TDB2Storage(DriverConfiguration configuration) {
        super(configuration);
        String targetDir = configuration.getStorageProperties().getPhysicalURI().toString();
        if (targetDir.startsWith(FILE_PREFIX)) {
            targetDir = configuration.getStorageProperties().getPhysicalURI().getSchemeSpecificPart();
        }
        Location location = Location.create(targetDir);
        this.dataset = TDB2Factory.connectDataset(location);
    }

    // Changes are written automatically on commit by TDB2
}
