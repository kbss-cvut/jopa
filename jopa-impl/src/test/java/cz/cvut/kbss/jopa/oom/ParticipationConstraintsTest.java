package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.exceptions.CardinalityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.model.OWLClassL;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.utils.TestEnvironmentUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

/**
 * Created by ledvima1 on 12.12.14.
 */
public class ParticipationConstraintsTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityL");

    @Mock
    private EntityType<OWLClassL> etLMock;

    @Mock
    private ListAttribute simpleListMock;

    @Mock
    private ListAttribute refListMock;

    @Mock
    private PluralAttribute setMock;

    @Mock
    private Attribute singleAMock;

    @Mock
    private Identifier idLMock;

    @Mock
    private EntityMappingHelper mapperMock;

    private Descriptor descriptor;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        TestEnvironmentUtils.initOWLClassLMocks(etLMock, refListMock, simpleListMock, setMock, singleAMock, idLMock);
        this.descriptor = new EntityDescriptor();
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsExceptionWhenMinimumCardinalityIsViolatedOnAttributeLoad() throws Exception {
        final FieldStrategy fs = FieldStrategy.createFieldStrategy(etLMock, simpleListMock, descriptor, mapperMock);
        final OWLClassL res = new OWLClassL();
        fs.buildInstanceFieldValue(res);
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsExceptionWhenMaximumCardinalityIsViolatedOnAttributeLoad() throws Exception {
        final FieldStrategy fs = FieldStrategy.createFieldStrategy(etLMock, refListMock, descriptor, mapperMock);
        final List<Axiom<NamedResource>> axioms = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityA_" + i));
            final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(PK),
                    Assertion.createObjectPropertyAssertion(refListMock.getOWLPropertyHasContentsIRI().toURI(), false),
                    new Value<NamedResource>(NamedResource.create(a.getUri())));
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(), descriptor)).thenReturn(a);
            axioms.add(ax);
        }
        when(mapperMock.loadReferencedList(any(ReferencedListDescriptor.class))).thenReturn(axioms);
        fs.addValueFromAxiom(axioms.get(0));
        final OWLClassL res = new OWLClassL();
        fs.buildInstanceFieldValue(res);
    }

    @Test
    public void loadsFieldWhenCardinalityConstraintsAreMet() throws Exception {
        final FieldStrategy fs = FieldStrategy.createFieldStrategy(etLMock, setMock, descriptor, mapperMock);
        final int count = 3;
        final List<OWLClassA> as = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            final OWLClassA a = new OWLClassA();
            a.setUri(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityA_" + i));
            as.add(a);
            final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(PK),
                    Assertion.createObjectPropertyAssertion(setMock.getIRI().toURI(), false),
                    new Value<NamedResource>(NamedResource.create(a.getUri())));
            when(mapperMock.getEntityFromCacheOrOntology(OWLClassA.class, a.getUri(), descriptor)).thenReturn(a);
            fs.addValueFromAxiom(ax);
        }
        final OWLClassL res = new OWLClassL();
        fs.buildInstanceFieldValue(res);

        assertFalse(res.getSet().isEmpty());
        assertEquals(count, res.getSet().size());
        assertTrue(res.getSet().containsAll(as));
    }

    @Test(expected = CardinalityConstraintViolatedException.class)
    public void throwsExceptionWhenMinimumCardinalityIsNotMetForSingleValueField() throws Exception {
        final FieldStrategy fs = FieldStrategy.createFieldStrategy(etLMock, singleAMock, descriptor, mapperMock);
        final OWLClassL res = new OWLClassL();
        fs.buildInstanceFieldValue(res);
    }

    // TODO Test also when data are being merge into ontology
}
