package cz.cvut.kbss.ontodriver.owlapi.list;

import cz.cvut.kbss.jopa.model.SequencesVocabulary;
import cz.cvut.kbss.ontodriver.owlapi.OwlapiAdapter;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.environment.TestUtils;
import cz.cvut.kbss.ontodriver_new.model.Assertion;
import cz.cvut.kbss.ontodriver_new.model.NamedResource;
import org.mockito.Mock;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

public abstract class ListHandlerTestBase {

    static final List<URI> LIST_ITEMS = initListItems();
    static final NamedResource SUBJECT = NamedResource
            .create("http://krizik.felk.cvut.cz/ontologies/jopa#Owner");
    static final Assertion HAS_LIST = Assertion
            .createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasListProperty), false);
    static final Assertion HAS_NEXT = Assertion
            .createObjectPropertyAssertion(URI.create(SequencesVocabulary.s_p_hasNext), false);

    OWLOntology ontology;
    OWLOntologyManager manager;
    OWLDataFactory dataFactory;
    OWLNamedIndividual individual;

    @Mock
    OWLReasoner reasonerMock;

    @Mock
    OwlapiAdapter adapterMock;

    private static List<URI> initListItems() {
        final List<URI> lst = new ArrayList<>(10);
        for (int i = 0; i < 10; i++) {
            lst.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#ListItem_" + i));
        }
        return lst;
    }

    public void setUp() throws Exception {
        final OntologyStructures realSnapshot = TestUtils.initRealOntology(reasonerMock);
        this.ontology = spy(realSnapshot.getOntology());
        this.manager = spy(realSnapshot.getOntologyManager());
        this.dataFactory = realSnapshot.getDataFactory();
        when(adapterMock.getLanguage()).thenReturn("en");
        this.individual = dataFactory.getOWLNamedIndividual(IRI.create(SUBJECT.getIdentifier()));
    }
}
