package cz.cvut.kbss.jopa.test.environment;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.ontodriver.sesame.util.SesameUtils;
import org.openrdf.model.ValueFactory;
import org.openrdf.repository.RepositoryConnection;

import java.net.URI;
import java.util.Collection;

public class SesameDataPersist {

    public void persistTestData(Collection<Triple> data, EntityManager em) throws Exception {
        final RepositoryConnection connection = em.unwrap(RepositoryConnection.class);
        final ValueFactory vf = connection.getValueFactory();
        connection.begin();
        for (Triple t : data) {
            if (t.getValue() instanceof URI) {
                connection.add(vf.createURI(t.getSubject().toString()), vf.createURI(t.getProperty().toString()),
                        vf.createURI(t.getValue().toString()));
            } else {
                connection.add(vf.createURI(t.getSubject().toString()), vf.createURI(t.getProperty().toString()),
                        SesameUtils.createDataPropertyLiteral(t.getValue(), "en", vf));
            }

        }
        connection.commit();
    }
}
