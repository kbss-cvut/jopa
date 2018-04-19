package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import org.apache.jena.query.Dataset;
import org.apache.jena.rdf.model.*;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.*;
import static org.junit.Assert.assertTrue;

public class JenaDataAccessor implements DataAccessor {

    @Override
    public void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        final StorageConnector ds = em.unwrap(StorageConnector.class);
        ds.begin();
        final List<Statement> toAdd = new ArrayList<>(data.size());
        for (Triple t : data) {
            final Resource subject = createResource(t.getSubject().toString());
            final Property property = createProperty(t.getProperty().toString());
            final RDFNode value;
            if (t.getValue() instanceof URI) {
                value = createResource(t.getValue().toString());
            } else {
                value = t.getValue() instanceof String ?
                        ResourceFactory.createLangLiteral(t.getValue().toString(), t.getLanguage()) :
                        ResourceFactory.createTypedLiteral(t.getValue());
            }
            toAdd.add(createStatement(subject, property, value));
        }
        ds.add(toAdd, null);
        ds.commit();
    }

    @Override
    public void verifyDataPresence(Collection<Triple> expected, EntityManager em) {
        final Dataset ds = em.unwrap(Dataset.class);
        final Model model = ds.getDefaultModel();
        for (Triple t : expected) {
            if (t.getValue() instanceof URI) {
                assertTrue(model.contains(createResource(t.getSubject().toString()),
                        createProperty(t.getProperty().toString()), createResource(t.getValue().toString())));
            } else {
                if (t.getLanguage() != null) {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()),
                            createLangLiteral(t.getValue().toString(), t.getLanguage())));
                } else {
                    assertTrue(model.contains(createResource(t.getSubject().toString()),
                            createProperty(t.getProperty().toString()), createTypedLiteral(t.getValue())));
                }
            }
        }
    }
}
