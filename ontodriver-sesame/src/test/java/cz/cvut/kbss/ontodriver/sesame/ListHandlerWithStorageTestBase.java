package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntoDriverProperties;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.After;
import org.junit.BeforeClass;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public abstract class ListHandlerWithStorageTestBase {

    protected static NamedResource OWNER = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityC");

    protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasListProperty";
    protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasNext";

    protected static OntologyStorageProperties storageProperties;
    protected static Map<String, String> properties;

    protected Connector connector;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        storageProperties = OntologyStorageProperties.physicalUri(URI.create("SesameListTest"))
                                                     .driver(SesameDataSource.class.getCanonicalName())
                                                     .build();
        properties = new HashMap<>();
        properties.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        properties.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
        properties.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        properties.put("cz.cvut.jopa.lang", "en");
    }

    @After
    public void tearDown() throws Exception {
        connector.close();
        ConnectorFactory.getInstance().close();
        final Field openField = ConnectorFactory.getInstance().getClass().getDeclaredField("open");
        openField.setAccessible(true);
        openField.set(ConnectorFactory.getInstance(), true);
    }

    protected void verifyListContent(Collection<Axiom<NamedResource>> expected,
                                     Collection<Axiom<NamedResource>> actual) throws Exception {
        assertEquals(expected.size(), actual.size());
        // This is more explicit on failure than just containsAll
        final Iterator<Axiom<NamedResource>> itExp = expected.iterator();
        final Iterator<Axiom<NamedResource>> itAct = actual.iterator();
        while (itExp.hasNext()) {
            assertEquals(itExp.next(), itAct.next());
        }
    }
}
