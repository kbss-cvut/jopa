package cz.cvut.kbss.ontodriver.sesame;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import cz.cvut.kbss.ontodriver.sesame.connector.ConnectorFactory;
import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

public class ReferencedListHandlerWithStorageTest extends ListHandlerWithStorageTestBase {

    protected static final String NODE_CONTENT_PROPERTY = "http://krizik.felk.cvut.cz/ontologies/2008/6/sequences.owl#hasContents";


    private ReferencedListHandler handler;

    @Before
    public void setUp() throws Exception {
        connector = ConnectorFactory.getInstance().createStorageConnector(storageProperties,
                properties);
        this.handler = new ReferencedListHandler(connector, connector.getValueFactory());
        connector.begin();
    }

    @Test
    public void persistsReferencedList() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(8);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    private ReferencedListValueDescriptor initValues(int count) {
        final ReferencedListValueDescriptor desc = new ReferencedListValueDescriptor(OWNER,
                Assertion.createObjectPropertyAssertion(URI.create(LIST_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NEXT_NODE_PROPERTY), false),
                Assertion.createObjectPropertyAssertion(URI.create(NODE_CONTENT_PROPERTY), false));
        for (int i = 0; i < count; i++) {
            desc.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#EntityA_" + i));
        }
        return desc;
    }

    private Collection<Axiom<NamedResource>> generateAxiomsForList(ReferencedListValueDescriptor listDescriptor) {
        final Collection<Axiom<NamedResource>> axioms = new ArrayList<>(listDescriptor.getValues().size());
        if (listDescriptor.getValues().isEmpty()) {
            return axioms;
        }
        int counter = 0;
        final String uriBase = OWNER.getIdentifier().toString();
        for (NamedResource val : listDescriptor.getValues()) {

            NamedResource node = NamedResource.create(uriBase + "-SEQ_" + counter++);
            Axiom<NamedResource> ax = new AxiomImpl<>(node, listDescriptor.getNodeContent(), new Value<>(val));
            axioms.add(ax);
        }
        return axioms;
    }

    @Test
    public void persistsEmptyList() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(0);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);

        handler.persistList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByPersistingValuesWhenTheOriginalWasEmpty() throws Exception {
        final ReferencedListValueDescriptor descriptor = initValues(5);
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        assertTrue(handler.loadList(descriptor).isEmpty());

        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByClearingAllValues() throws Exception {
        persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        handler.updateList(updated);
        connector.commit();
        connector.begin();
        assertTrue(handler.loadList(updated).isEmpty());
    }

    private ReferencedListValueDescriptor persistOriginalList() throws Exception {
        final ReferencedListValueDescriptor original = initValues(10);
        handler.persistList(original);
        connector.commit();
        connector.begin();
        assertFalse(handler.loadList(original).isEmpty());
        return original;
    }

    @Test
    public void updatesListByAppendingSeveralNewValues() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (NamedResource r : original.getValues()) {
            updated.addValue(r);
        }
        for (int i = 0; i < 5; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#Appended_" + i));
        }
        updateAndCheck(updated);
    }

    private void updateAndCheck(ReferencedListValueDescriptor descriptor) throws Exception {
        final Collection<Axiom<NamedResource>> axioms = generateAxiomsForList(descriptor);
        handler.updateList(descriptor);
        connector.commit();
        connector.begin();
        verifyListContent(axioms, handler.loadList(descriptor));
    }

    @Test
    public void updatesListByRemovingSeveralValuesFromTheEnd() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() / 2; i++) {
            updated.addValue(original.getValues().get(i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByReplacingSomeElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
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
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
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
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size() + 2; i++) {
            updated.addValue(NamedResource
                    .create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#REplacement_" + i));
        }
        updateAndCheck(updated);
    }

    @Test
    public void updatesListByRemovingSomeOfTheElements() throws Exception {
        final ReferencedListValueDescriptor original = persistOriginalList();

        final ReferencedListValueDescriptor updated = initValues(0);
        for (int i = 0; i < original.getValues().size(); i++) {
            if (i % 2 != 0) {
                updated.addValue(original.getValues().get(i));
            }
        }
        updateAndCheck(updated);
    }

}
