package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.junit.Before;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;

public class ReferencedListIteratorTest extends OwlapiListIteratorBase {

    private ReferencedListDescriptor descriptor;

    @Before
    public void setUp() throws Exception {
        super.setUp();
        final OWLNamedIndividual individual = snapshot.getDataFactory().getOWLNamedIndividual(IRI.create(SUBJECT));
        this.testHelper = new ReferencedListTestHelper(snapshot, individual, SUBJECT);
        this.descriptor = new ReferencedListDescriptorImpl(NamedResource.create(SUBJECT), ListHandlerTestBase.HAS_LIST,
                ListHandlerTestBase.HAS_NEXT, ListHandlerTestBase.HAS_CONTENT);
        this.iterator = iterator();
    }

    @Override
    OwlapiListIterator iterator() {
        return new ReferencedListIterator(descriptor, snapshot, axiomAdapter);
    }
}
