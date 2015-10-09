package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import cz.cvut.kbss.ontodriver_new.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.net.URI;

import static org.mockito.Matchers.anyList;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

public class AxiomSaverTest {

    private static final NamedResource INDIVIDUAL = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa/Individual");
    private static final URI ASSERTION_URI = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/TestAssertion");

    @Mock
    private OwlapiAdapter adapterMock;

    @Mock
    private OWLOntology ontologyMock;
    @Mock
    private OWLOntologyManager managerMock;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory dataFactory;

    private AxiomValueDescriptor descriptor;

    private AxiomSaver axiomSaver;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.dataFactory = new OWLDataFactoryImpl();
        this.axiomSaver = new AxiomSaver(adapterMock, new OntologyStructures(ontologyMock, managerMock, dataFactory,
                reasonerMock), "en");
        this.descriptor = new AxiomValueDescriptor(INDIVIDUAL);
    }

    @Test
    public void persistValueWithNullDoesNothing() throws Exception {
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(ASSERTION_URI, false);
        final Assertion opAssertion = Assertion.createObjectPropertyAssertion(ASSERTION_URI, false);
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(ASSERTION_URI, false);
        descriptor.addAssertionValue(dpAssertion, Value.nullValue());
        descriptor.addAssertionValue(opAssertion, Value.nullValue());
        descriptor.addAssertionValue(apAssertion, Value.nullValue());
        axiomSaver.persist(descriptor);
        verify(managerMock, never()).applyChanges(anyList());
    }
}