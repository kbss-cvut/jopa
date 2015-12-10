package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptorImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.junit.Before;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class SimpleListIteratorTest extends OwlapiListIteratorBase {

    private SimpleListDescriptor descriptor;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        final OWLNamedIndividual individual = snapshot.getDataFactory().getOWLNamedIndividual(IRI.create(SUBJECT));
        this.testHelper = new SimpleListTestHelper(snapshot, individual);
        this.descriptor = new SimpleListDescriptorImpl(NamedResource.create(SUBJECT), ListHandlerTestBase.HAS_LIST,
                ListHandlerTestBase.HAS_NEXT);
        this.iterator = iterator();
    }

    @Override
    protected OwlapiListIterator iterator() {
        return new SimpleListIterator(descriptor, snapshot, axiomAdapter);
    }
}