package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.OntologyConnectorType;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.sesame.connector.Connector;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import cz.cvut.kbss.ontodriver_new.OntoDriverProperties;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.openrdf.model.ValueFactory;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;

/**
 * We'll want to use the same test code for referenced list as well. TODO
 * Refactoring
 *
 * @author ledvima1
 */
public class SimpleListHandlerTestWithStorage {

    private static NamedResource OWNER = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityC");

    protected static final String LIST_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleSequence";
    protected static final String NEXT_NODE_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#C-hasSimpleNext";

    private static OntologyStorageProperties storageProperties;
    private static Map<String, String> properties;

    private Connector connector;

    private SimpleListHandler handler;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        storageProperties = OntologyStorageProperties.connectorType(OntologyConnectorType.SESAME)
                .physicalUri(URI.create("SesameSimpleListTest"))
                .ontologyUri(URI.create("http://krizik.felk.cvut.cz/ontologies/simpleList"))
                .build();
        properties = new HashMap<>();
        properties.put(OntoDriverProperties.SESAME_USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        properties.put(OntoDriverProperties.SESAME_USE_INFERENCE, Boolean.FALSE.toString());
        properties.put(OntoDriverProperties.USE_TRANSACTIONAL_ONTOLOGY, Boolean.TRUE.toString());
        properties.put("cz.cvut.jopa.lang", "en");
    }

    @Before
    public void setUp() throws Exception {
        connector = ConnectorFactory.getInstance().createStorageConnector(storageProperties,
                properties);
        this.handler = new SimpleListHandler(connector, connector.getValueFactory());
        connector.begin();
    }

    @After
    public void tearDown() throws Exception {
        connector.close();
        ConnectorFactory.getInstance().close();
        final Field openField = ConnectorFactory.getInstance().getClass().getDeclaredField("open");
        openField.setAccessible(true);
        openField.set(ConnectorFactory.getInstance(), true);
    }

    @Test
    public void persistsSimpleList() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(8);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, descriptor);
    }

    private SimpleListValueDescriptor initValues(int count) {
        final SimpleListValueDescriptor desc = new SimpleListValueDescriptor(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityA_" + i));
        }
        return desc;
    }

    private Collection<Axiom<NamedResource>> generateAxiomsForList(SimpleListValueDescriptor listDescriptor) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>(listDescriptor.getValues().size());
        if (listDescriptor.getValues().isEmpty()) {
            return axioms;
        }
        NamedResource previous = listDescriptor.getValues().get(0);
        for (NamedResource val : listDescriptor.getValues()) {
            Axiom<NamedResource> ax;
            if (val == previous) {
                ax = new AxiomImpl<>(OWNER, listDescriptor.getListProperty(), new Value<>(val));
            } else {
                ax = new AxiomImpl<>(previous, listDescriptor.getNextNode(), new Value<>(val));
            }
            axioms.add(ax);
            previous = val;
        }
        return axioms;
    }

    private void verifyListContent(Collection<Axiom<NamedResource>> expected,
                                   SimpleListDescriptor listDescriptor) throws Exception {
        final Collection<Axiom<NamedResource>> actual = handler.loadList(listDescriptor);
        assertEquals(expected.size(), actual.size());
        // This is more explicit on failure than just containsAll
        final Iterator<Axiom<NamedResource>> itExp = expected.iterator();
        final Iterator<Axiom<NamedResource>> itAct = actual.iterator();
        while (itExp.hasNext()) {
            assertEquals(itExp.next(), itAct.next());
        }
    }

    @Test
    public void persistsEmptyList() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(0);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, descriptor);
    }

    @Test
    public void updatesListByPersistingValuesWhenTheOriginalWasEmpty() throws Exception {
        final SimpleListValueDescriptor descriptor = initValues(5);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        assertTrue(handler.loadList(descriptor).isEmpty());

        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, descriptor);
    }

    @Test
    public void updatesListByClearingAllValues() throws Exception {
        persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        handler.updateList(updated);
        connector.commit();
        connector.begin();
        assertTrue(handler.loadList(updated).isEmpty());
    }

    private SimpleListValueDescriptor persistOriginalList() throws Exception {
        final SimpleListValueDescriptor original = initValues(10);
        handler.persistList(original);
        connector.commit();
        connector.begin();
        assertFalse(handler.loadList(original).isEmpty());
        return original;
    }

    @Test
    public void updatesListByAppendingSeveralNewValues() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (NamedResource r : original.getValues()) {
            updated.addValue(r);
        }
        for (int i = 0; i < 5; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Appended_" + i));
        }
        updateAndCheck(updated);
    }

    private void updateAndCheck(SimpleListValueDescriptor descriptor) throws Exception {
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, descriptor);
    }

    @Test
    public void updatesListByRemovingSeveralValuesFromTheEnd() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() / 2; i++) {
            updated.addValue(original.getValues().get(i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingSomeElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(NamedResource
                        .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Modified_" + i));
            } else {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByPrependingSeveralNewElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < 4; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Prepended_" + i));
        }
        for (NamedResource elem : original.getValues()) {
            updated.addValue(elem);
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingTheWholeListWithNewElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() + 2; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#REplacement_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSomeOfTheElements() throws Exception {
        final SimpleListValueDescriptor original = persistOriginalList();

        final SimpleListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }
}
