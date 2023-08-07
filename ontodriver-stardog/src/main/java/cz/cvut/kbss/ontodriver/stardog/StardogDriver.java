package cz.cvut.kbss.ontodriver.stardog;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.Rdf4jDriver;
import cz.cvut.kbss.ontodriver.rdf4j.connector.init.FactoryOfFactories;
import cz.cvut.kbss.ontodriver.rdf4j.exception.Rdf4jDriverException;
import cz.cvut.kbss.ontodriver.stardog.connector.init.StardogFactoryOfFactories;

import java.util.Map;

public class StardogDriver extends Rdf4jDriver {

    protected StardogDriver(OntologyStorageProperties storageProperties,
                            Map<String, String> properties) throws Rdf4jDriverException {
        super(storageProperties, properties);
    }

    @Override
    protected FactoryOfFactories getFactoryOfFactories(DriverConfiguration config) throws Rdf4jDriverException {
        return new StardogFactoryOfFactories(config);
    }
}
