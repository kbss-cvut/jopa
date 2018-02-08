package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.jena.JenaDataSource;
import org.apache.jena.rdf.model.Resource;
import org.apache.jena.rdf.model.ResourceFactory;
import org.apache.jena.rdf.model.Statement;

import java.net.URI;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;

class StorageTestUtil {

    static final String SUBJECT = "http://onto.fel.cvut.cz/ontologies/jena-driver/Resource";
    static final String TYPE_ONE = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeOne";
    static final String TYPE_TWO = "http://onto.fel.cvut.cz/ontologies/jena-driver/TypeTwo";
    static final String NAMED_GRAPH = "http://onto.fel.cvut.cz/ontologies/jena-driver/GraphOne";

    static final Resource RESOURCE = ResourceFactory.createResource(SUBJECT);

    static Configuration createConfiguration(String physicalUri) {
        return new Configuration(OntologyStorageProperties.driver(JenaDataSource.class.toString()).physicalUri(
                URI.create(physicalUri)).build());
    }

    static Statement statement(String subject, String property, String value) {
        return ResourceFactory
                .createStatement(createResource(subject), createProperty(property), createResource(value));
    }
}
