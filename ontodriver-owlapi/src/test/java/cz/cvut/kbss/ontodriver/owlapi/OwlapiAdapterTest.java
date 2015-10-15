package cz.cvut.kbss.ontodriver.owlapi;

import com.google.common.base.Optional;
import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import cz.cvut.kbss.ontodriver.owlapi.connector.OntologyStructures;
import cz.cvut.kbss.ontodriver.owlapi.exceptions.InvalidOntologyIriException;
import cz.cvut.kbss.ontodriver.owlapi.util.MutableAddAxiom;
import cz.cvut.kbss.ontodriver.owlapi.util.OwlapiUtils;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.descriptors.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver_new.exception.OWLIndividualExistsException;
import cz.cvut.kbss.ontodriver_new.model.*;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNodeSet;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class OwlapiAdapterTest {

    private static final URI PK = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#EntityA");

    @Mock
    private Connector connectorMock;
    @Mock
    private OntologyStructures snapshotMock;
    @Mock
    private OWLOntology ontologyMock;
    @Mock
    private OWLReasoner reasonerMock;

    private OWLDataFactory factory;

    private OntologyStructures realSnapshot;

    private OwlapiAdapter adapter;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(connectorMock.getOntologySnapshot()).thenReturn(snapshotMock);
        when(snapshotMock.getOntology()).thenReturn(ontologyMock);
        this.factory = new OWLDataFactoryImpl();
        when(snapshotMock.getDataFactory()).thenReturn(factory);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);

        this.adapter = spy(new OwlapiAdapter(connectorMock, Collections.emptyMap()));
    }

    @Test
    public void commitSendsChangesToConnector() throws Exception {
        startTransaction();
        generateChange();
        adapter.commit();
        verify(connectorMock).applyChanges(any());
    }

    private void generateChange() throws Exception {
        final Field changesField = OwlapiAdapter.class.getDeclaredField("pendingChanges");
        changesField.setAccessible(true);
        final List<OWLOntologyChange> changes = (List<OWLOntologyChange>) changesField.get(adapter);
        changes.add(mock(OWLOntologyChange.class));
    }

    private void startTransaction() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Method startTransaction = OwlapiAdapter.class.getDeclaredMethod("startTransactionIfNotActive");
        startTransaction.setAccessible(true);
        startTransaction.invoke(adapter);
    }

    @Test
    public void rollbackCausesChangesToEmpty() throws Exception {
        startTransaction();
        generateChange();
        adapter.rollback();
        adapter.commit();
        verify(connectorMock, never()).applyChanges(any());
    }

    @Test
    public void testIsConsistentWithCorrectContext() throws Exception {
        final IRI iri = setupOntologyIri();
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isConsistent()).thenReturn(Boolean.TRUE);

        assertTrue(adapter.isConsistent(iri.toURI()));
        verify(reasonerMock).isConsistent();
    }

    private IRI setupOntologyIri() {
        final IRI iri = IRI.create("http://krizik.felk.cvut.cz/ontologies/jopa");
        final OWLOntologyID id = new OWLOntologyID(Optional.fromNullable(iri), Optional.absent());
        when(ontologyMock.getOntologyID()).thenReturn(id);
        return iri;
    }

    @Test(expected = InvalidOntologyIriException.class)
    public void isConsistentThrowsExceptionOnInvalidContext() throws Exception {
        setupOntologyIri();
        final URI ctx = URI.create("http://krizik.felk.cvut.cz/differentContext");
        try {
            adapter.isConsistent(ctx);
        } finally {
            verify(snapshotMock, never()).getReasoner();
        }
    }

    @Test
    public void testGetContexts() throws Exception {
        final IRI iri = setupOntologyIri();

        final List<URI> res = adapter.getContexts();
        assertEquals(1, res.size());
        assertEquals(iri.toURI(), res.get(0));
    }

    @Test
    public void testContainsUnknownContext() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        final URI context = URI.create("http://krizik.felk.cvut.cz/jopa/different");
        setupOntologyIri();
        boolean res = adapter.containsAxiom(axiom, context);
        assertFalse(res);
    }

    private Axiom<?> initAxiomForContains(Assertion.AssertionType assType, boolean inferred) {
        final NamedResource individual = NamedResource.create(
                "http://krizik.felk.cvut.cz/ontologies/jopa#IndividualOne");
        Assertion assertion = null;
        Value<?> value = null;
        switch (assType) {
            case CLASS:
                assertion = Assertion.createClassAssertion(inferred);
                value = new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#typeOne"));
                break;
            case PROPERTY:
            case OBJECT_PROPERTY:
                assertion = Assertion.createObjectPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#objectProperty"), inferred);
                value = new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#IndividualTwo"));
                break;
            case DATA_PROPERTY:
                assertion = Assertion.createDataPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#dataProperty"), inferred);
                value = new Value<>("StringDataPropertyValue");
                break;
            case ANNOTATION_PROPERTY:
                assertion = Assertion.createAnnotationPropertyAssertion(
                        URI.create("http://krizik.felk.cvut.cz/ontologies/jopa#annotationProperty"), inferred);
                value = new Value<>("This is annotation value");
                break;
        }
        return new AxiomImpl<>(individual, assertion, value);
    }

    @Test
    public void testContainsClassAssertionAxiomNoContext() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.CLASS, false);
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontologyMock).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.CLASS_ASSERTION));
        final Set<OWLClass> types = ax.getClassesInSignature();
        assertEquals(1, types.size());
        final OWLClass cls = types.iterator().next();
        assertEquals(axiom.getValue().getValue(), cls.getIRI().toURI());
    }

    @Test
    public void testContainsObjectPropertyAssertionInferred() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.OBJECT_PROPERTY, true);
        final IRI ctx = setupOntologyIri();
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.FALSE);
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, ctx.toURI());

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.OBJECT_PROPERTY_ASSERTION));
        final Set<OWLNamedIndividual> individuals = ax.getIndividualsInSignature();
        assertEquals(2, individuals.size());
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getSubject().toString()))));
        assertTrue(individuals.contains(factory.getOWLNamedIndividual(IRI.create(axiom.getValue().stringValue()))));
        final Set<OWLObjectProperty> objProperties = ax.getObjectPropertiesInSignature();
        assertEquals(1, objProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), objProperties.iterator().next().getIRI().toURI());
    }

    @Test
    public void testContainsDataProperty() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.DATA_PROPERTY, false);
        when(ontologyMock.containsAxiom(any())).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(ontologyMock).containsAxiom(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.DATA_PROPERTY_ASSERTION));
        final Set<OWLDataProperty> dataProperties = ax.getDataPropertiesInSignature();
        assertEquals(1, dataProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), dataProperties.iterator().next().getIRI().toURI());
        final Set<OWLNamedIndividual> individuals = ax.getIndividualsInSignature();
        assertEquals(1, individuals.size());
        assertEquals(axiom.getSubject().getIdentifier(), individuals.iterator().next().getIRI().toURI());
        assertTrue(ax instanceof OWLDataPropertyAssertionAxiom);
        assertEquals(axiom.getValue().stringValue(), ((OWLDataPropertyAssertionAxiom) ax).getObject().getLiteral());
    }

    @Test
    public void testContainsAnnotationPropertyInferred() throws Exception {
        final Axiom<?> axiom = initAxiomForContains(Assertion.AssertionType.ANNOTATION_PROPERTY, true);
        setupOntologyIri();
        final OWLReasoner reasonerMock = mock(OWLReasoner.class);
        when(snapshotMock.getReasoner()).thenReturn(reasonerMock);
        when(reasonerMock.isEntailed(any(OWLAxiom.class))).thenReturn(Boolean.TRUE);
        boolean res = adapter.containsAxiom(axiom, null);

        assertTrue(res);
        final ArgumentCaptor<OWLAxiom> captor = ArgumentCaptor.forClass(OWLAxiom.class);
        verify(reasonerMock).isEntailed(captor.capture());
        final OWLAxiom ax = captor.getValue();
        assertTrue(ax.isOfType(AxiomType.ANNOTATION_ASSERTION));
        final Set<OWLAnnotationProperty> annotationProperties = ax.getAnnotationPropertiesInSignature();
        assertEquals(1, annotationProperties.size());
        assertEquals(axiom.getAssertion().getIdentifier(), annotationProperties.iterator().next().getIRI().toURI());
        assertTrue(ax instanceof OWLAnnotationAssertionAxiom);
        final OWLAnnotationAssertionAxiom assertionAxiom = (OWLAnnotationAssertionAxiom) ax;
        assertEquals(axiom.getValue().stringValue(),
                assertionAxiom.getAnnotation().getValue().asLiteral().get().getLiteral());
    }

    @Test
    public void findReturnsEmptyCollectionWhenIndividualIsNotInOntologySignature() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        initRealOntology();
        final Collection<Axiom<?>> res = adapter.find(descriptor);
        assertTrue(res.isEmpty());
    }

    private void initRealOntology() throws Exception {
        final OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        final OWLOntology ontology = manager.createOntology(
                IRI.create("http://krizik.felk.cvut.cz/ontologies/adapterTest"));
        this.realSnapshot = new OntologyStructures(ontology, manager, manager.getOWLDataFactory(), reasonerMock);
        when(connectorMock.getOntologySnapshot()).thenReturn(realSnapshot);
    }

    private void addClassAssertionsToOntology(URI subject, Collection<URI> classes) {
        final OWLNamedIndividual individual = realSnapshot.getDataFactory().getOWLNamedIndividual(IRI.create(subject));
        classes.stream().forEach(uri -> {
            final OWLClass cls = realSnapshot.getDataFactory().getOWLClass(IRI.create(uri));
            final OWLClassAssertionAxiom a = realSnapshot.getDataFactory().getOWLClassAssertionAxiom(cls, individual);
            realSnapshot.getOntologyManager().applyChange(new AddAxiom(realSnapshot.getOntology(), a));
        });
    }

    @Test
    public void testFindDataPropertyAssertions() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI dataProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute");
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(dataProperty, false));
        final Set<String> values = new HashSet<>(Arrays.asList("stringOne", "stringTwo"));
        addDataPropertyAssertionsToOntology(PK, Collections.singletonMap(dataProperty, values));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        verifyDataPropertyAxioms(values, res);
    }

    private void verifyDataPropertyAxioms(Set<String> values, Collection<Axiom<?>> res) {
        assertEquals(values.size(), res.size());
        res.stream().forEach(ax -> {
            final Object val = ax.getValue().getValue();
            assertTrue(values.contains(val.toString()));
        });
    }

    private void addDataPropertyAssertionsToOntology(URI subject, Map<URI, Collection<?>> values) {
        final OWLNamedIndividual individual = realSnapshot.getDataFactory().getOWLNamedIndividual(IRI.create(subject));
        for (Map.Entry<URI, Collection<?>> e : values.entrySet()) {
            final OWLDataProperty dp = realSnapshot.getDataFactory().getOWLDataProperty(IRI.create(e.getKey()));
            e.getValue().stream().forEach(value -> {
                final OWLDataPropertyAssertionAxiom a = realSnapshot.getDataFactory().getOWLDataPropertyAssertionAxiom(
                        dp, individual, value.toString());
                realSnapshot.getOntologyManager().applyChange(new AddAxiom(realSnapshot.getOntology(), a));
            });
        }
    }

    @Test
    public void testFindObjectPropertyAssertions() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI objectProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA");
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(objectProperty, false));
        final Set<NamedResource> values = new HashSet<>();
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        addObjectPropertyAssertionsToOntology(PK, Collections.singletonMap(objectProperty, values));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        verifyObjectPropertyAxioms(values, res);
    }

    private void addObjectPropertyAssertionsToOntology(URI subject, Map<URI, Collection<NamedResource>> values) {
        final OWLNamedIndividual individual = realSnapshot.getDataFactory().getOWLNamedIndividual(IRI.create(subject));
        for (Map.Entry<URI, Collection<NamedResource>> e : values.entrySet()) {
            final OWLObjectProperty op = realSnapshot.getDataFactory().getOWLObjectProperty(IRI.create(e.getKey()));
            e.getValue().stream().forEach(value -> {
                final OWLNamedIndividual target = realSnapshot.getDataFactory().getOWLNamedIndividual(
                        IRI.create(value.getIdentifier()));
                final OWLObjectPropertyAssertionAxiom a = realSnapshot.getDataFactory()
                                                                      .getOWLObjectPropertyAssertionAxiom(
                                                                              op, individual, target);
                realSnapshot.getOntologyManager().applyChange(new AddAxiom(realSnapshot.getOntology(), a));
            });
        }
    }

    private void verifyObjectPropertyAxioms(Set<NamedResource> values, Collection<Axiom<?>> axioms) {
        assertEquals(values.size(), axioms.size());
        axioms.stream().forEach(axiom -> {
            final Object val = axiom.getValue().getValue();
            assertTrue(val instanceof NamedResource);
            assertTrue(values.contains(val));
        });
    }

    @Test
    public void testFindAnnotationPropertyAssertions() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI annotationProperty = URI.create(
                "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationLabel");
        descriptor.addAssertion(Assertion.createAnnotationPropertyAssertion(annotationProperty, false));
        final Set<Object> values = new HashSet<>(Arrays.asList("labelOne", "labelTwo"));
        addAnnotationPropertyAssertionsToOntology(PK, Collections.singletonMap(annotationProperty, values));
        // This just makes sure that the individual exists in the ontology
        addClassAssertionsToOntology(PK,
                Collections.singleton(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        assertEquals(values.size(), res.size());
        res.stream().filter(ax -> ax.getAssertion().getType() == Assertion.AssertionType.ANNOTATION_PROPERTY)
           .forEach(
                   ax -> {
                       final Object val = ax.getValue().getValue();
                       assertTrue(values.contains(val.toString()));
                   });
    }

    private void addAnnotationPropertyAssertionsToOntology(URI subject, Map<URI, Collection<Object>> values) {
        final OWLNamedIndividual individual = realSnapshot.getDataFactory().getOWLNamedIndividual(IRI.create(subject));
        for (Map.Entry<URI, Collection<Object>> e : values.entrySet()) {
            final OWLAnnotationProperty ap = realSnapshot.getDataFactory().getOWLAnnotationProperty(IRI.create(
                    e.getKey()));
            e.getValue().stream().forEach(value -> {
                OWLAnnotationValue annotationValue;
                if (value instanceof URI) {
                    annotationValue = IRI.create((URI) value);
                } else {
                    annotationValue = realSnapshot.getDataFactory().getOWLLiteral(value.toString());
                }
                final OWLAnnotationAssertionAxiom a = realSnapshot.getDataFactory().getOWLAnnotationAssertionAxiom(ap,
                        individual.getIRI(), annotationValue);
                realSnapshot.getOntologyManager().applyChange(new AddAxiom(realSnapshot.getOntology(), a));
            });
        }
    }

    @Test
    public void testFindMultipleAssertions() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        descriptor.addAssertion(Assertion.createClassAssertion(false));
        final Set<URI> classes = new HashSet<>();
        classes.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA"));
        addClassAssertionsToOntology(PK, classes);
        final URI dataProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute");
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(dataProperty, false));
        final Set<String> dpValues = new HashSet<>(Arrays.asList("stringOne", "stringTwo"));
        addDataPropertyAssertionsToOntology(PK, Collections.singletonMap(dataProperty, dpValues));
        final URI objectProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA");
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(objectProperty, false));
        final Set<NamedResource> opValues = new HashSet<>();
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        addObjectPropertyAssertionsToOntology(PK, Collections.singletonMap(objectProperty, opValues));
        final URI annotationProperty = URI.create(
                "http://krizik.felk.cvut.cz/ontologies/jopa/attributes#annotationLabel");
        descriptor.addAssertion(Assertion.createAnnotationPropertyAssertion(annotationProperty, false));
        final Set<Object> apValues = new HashSet<>(Arrays.asList("labelOne", "labelTwo"));
        addAnnotationPropertyAssertionsToOntology(PK, Collections.singletonMap(annotationProperty, apValues));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        assertEquals(classes.size() + dpValues.size() + opValues.size() + apValues.size(), res.size());
    }

    @Test
    public void testFindDataPropertyAssertionsWithInferred() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI dataProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-stringAttribute");
        descriptor.addAssertion(Assertion.createDataPropertyAssertion(dataProperty, true));
        final Set<String> values = new HashSet<>(Arrays.asList("stringOne", "stringTwo"));
        addDataPropertyAssertionsToOntology(PK, Collections.singletonMap(dataProperty, values));
        values.add("stringThree");
        when(reasonerMock.getDataPropertyValues(factory.getOWLNamedIndividual(IRI.create(PK)),
                factory.getOWLDataProperty(IRI.create(dataProperty)))).thenReturn(prepareLiteralsFromValues(values));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        verifyDataPropertyAxioms(values, res);
    }

    private Set<OWLLiteral> prepareLiteralsFromValues(Set<String> values) {
        return values.stream().map(factory::getOWLLiteral).collect(Collectors.toSet());
    }

    @Test
    public void testFindObjectPropertyAssertionsWithInferred() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI objectProperty = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA");
        descriptor.addAssertion(Assertion.createObjectPropertyAssertion(objectProperty, true));
        final Set<NamedResource> values = new HashSet<>();
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        addObjectPropertyAssertionsToOntology(PK, Collections.singletonMap(objectProperty, values));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        when(reasonerMock.getObjectPropertyValues(factory.getOWLNamedIndividual(IRI.create(PK)),
                factory.getOWLObjectProperty(IRI.create(objectProperty)))).thenReturn(
                prepareIndividualsFromUris(values));

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        verifyObjectPropertyAxioms(values, res);
    }

    private NodeSet<OWLNamedIndividual> prepareIndividualsFromUris(Set<NamedResource> uris) {
        final Set<Node<OWLNamedIndividual>> nodes = uris.stream().map(
                resource -> new OWLNamedIndividualNode(
                        factory.getOWLNamedIndividual(IRI.create(resource.getIdentifier())))).collect(
                Collectors.toSet());
        return new OWLNamedIndividualNodeSet(nodes);
    }

    @Test
    public void testFindUntypedPropertyAssertionWithInferred() throws Exception {
        final AxiomDescriptor descriptor = new AxiomDescriptor(NamedResource.create(PK));
        initRealOntology();
        final URI property = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA");
        // The property is object property, but the descriptor does not specify that so it will be treated as unspecified type
        descriptor.addAssertion(Assertion.createPropertyAssertion(property, true));
        final Set<NamedResource> values = new HashSet<>();
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        addObjectPropertyAssertionsToOntology(PK, Collections.singletonMap(property, values));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        when(reasonerMock.getObjectPropertyValues(factory.getOWLNamedIndividual(IRI.create(PK)),
                factory.getOWLObjectProperty(IRI.create(property)))).thenReturn(
                prepareIndividualsFromUris(values));
        when(reasonerMock.getTypes(factory.getOWLNamedIndividual(IRI.create(PK)), false)).thenReturn(
                new OWLClassNodeSet());

        final Collection<Axiom<?>> res = adapter.find(descriptor);
        verifyObjectPropertyAxioms(values, res);
    }

    private OWLNamedIndividual individual() {
        return factory.getOWLNamedIndividual(IRI.create(PK));
    }

    @Test
    public void testPersistIndividualWithDataProperty() throws Exception {
        initRealOntology();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(NamedResource.create(PK));
        final Assertion dpAssertion = Assertion.createDataPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-dataAttribute"), false);
        final Set<Object> values = new HashSet<>();
        values.add("StringValue");
        values.add(1111);
        values.add(3.14159D);
        descriptor.addAssertion(dpAssertion);
        values.forEach(value -> descriptor.addAssertionValue(dpAssertion, new Value<>(value)));

        adapter.persist(descriptor);
        verifyDataPropertyValuesInOntology(dpAssertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyTransactionalChangesRecorded() {
        final ArgumentCaptor<List> changesCaptor = ArgumentCaptor.forClass(List.class);
        verify(adapter, atLeastOnce()).addTransactionalChanges(changesCaptor.capture());
        final List<?> changes = changesCaptor.getValue();
        for (Object ch : changes) {
            assertTrue(ch instanceof MutableAddAxiom);
        }
    }

    private void verifyDataPropertyValuesInOntology(Assertion assertion, Set<Object> values) {
        final Set<OWLDataPropertyAssertionAxiom> dpAxioms = realSnapshot.getOntology().getDataPropertyAssertionAxioms(
                individual());
        assertEquals(values.size(), dpAxioms.size());
        for (OWLDataPropertyAssertionAxiom axiom : dpAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().asOWLDataProperty().getIRI().toURI());
            final Object value = OwlapiUtils.owlLiteralToValue(axiom.getObject());
            assertTrue(values.contains(value));
        }
    }

    @Test
    public void testPersistIndividualWithAnnotationProperty() throws Exception {
        initRealOntology();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(NamedResource.create(PK));
        // This just makes sure that the individual explicitly exists in the ontology
        descriptor.addAssertionValue(Assertion.createClassAssertion(false),
                new Value<>(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/entities#OWLClassA")));
        final Assertion apAssertion = Assertion.createAnnotationPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-annotationAttribute"), false);
        final Set<Object> values = new HashSet<>();
        values.add("StringLabel");
        values.add(1111);
        values.add(URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/apValue"));
        descriptor.addAssertion(apAssertion);
        values.forEach(value -> descriptor.addAssertionValue(apAssertion, new Value<>(value)));

        adapter.persist(descriptor);
        verifyAnnotationPropertyValuesInOntology(apAssertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyAnnotationPropertyValuesInOntology(Assertion assertion, Set<Object> values) {
        final Set<OWLAnnotationAssertionAxiom> apAxioms = realSnapshot.getOntology().getAnnotationAssertionAxioms(
                individual().getIRI());
        assertEquals(values.size(), apAxioms.size());
        for (OWLAnnotationAssertionAxiom axiom : apAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().getIRI().toURI());
            final OWLAnnotationValue value = axiom.getValue();
            if (value.asIRI().isPresent()) {
                assertTrue(values.contains(value.asIRI().get().toURI()));
            } else {
                final Object v = OwlapiUtils.owlLiteralToValue(value.asLiteral().get());
                assertTrue(values.contains(v));
            }
        }
    }

    @Test
    public void testPersistIndividualWithObjectProperty() throws Exception {
        initRealOntology();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(NamedResource.create(PK));
        final Assertion assertion = Assertion.createObjectPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(assertion);
        final Set<NamedResource> values = new HashSet<>();
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        values.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        values.forEach(value -> descriptor.addAssertionValue(assertion, new Value<>(value)));

        adapter.persist(descriptor);
        verifyObjectPropertyValuesInOntology(assertion, values);
        verifyTransactionalChangesRecorded();
    }

    private void verifyObjectPropertyValuesInOntology(Assertion assertion, Set<NamedResource> values) {
        final Set<OWLObjectPropertyAssertionAxiom> opAxioms = realSnapshot.getOntology()
                                                                          .getObjectPropertyAssertionAxioms(
                                                                                  individual());
        assertEquals(values.size(), opAxioms.size());
        for (OWLObjectPropertyAssertionAxiom axiom : opAxioms) {
            assertEquals(assertion.getIdentifier(), axiom.getProperty().asOWLObjectProperty().getIRI().toURI());
            final NamedResource target = NamedResource.create(
                    axiom.getObject().asOWLNamedIndividual().getIRI().toURI());
            assertTrue(values.contains(target));
        }
    }

    @Test
    public void testPersistIndividualWithUnspecifiedPropertyTypePropertiesExist() throws Exception {
        initRealOntology();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(NamedResource.create(PK));
        final Assertion opAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(opAssertion);
        final Set<NamedResource> opValues = new HashSet<>();
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        opValues.forEach(value -> descriptor.addAssertionValue(opAssertion, new Value<>(value)));
        final Assertion dpAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#A-dataAttribute"), false);
        final Set<Object> dpValues = new HashSet<>();
        dpValues.add("StringValue");
        descriptor.addAssertion(dpAssertion);
        dpValues.forEach(value -> descriptor.addAssertionValue(dpAssertion, new Value<>(value)));
        // Make sure that the assertions exist in the ontology so that the saver is able to determine the property type
        addDataPropertyAssertion(dpAssertion.getIdentifier());
        addObjectPropertyAssertion(opAssertion.getIdentifier());

        adapter.persist(descriptor);
        verifyDataPropertyValuesInOntology(dpAssertion, dpValues);
        verifyObjectPropertyValuesInOntology(opAssertion, opValues);
        verifyTransactionalChangesRecorded();
    }

    private void addDataPropertyAssertion(URI property) {
        final OWLDataProperty dataProperty = factory.getOWLDataProperty(IRI.create(property));
        final OWLIndividual ind = factory.getOWLAnonymousIndividual();
        final AddAxiom ax = new AddAxiom(realSnapshot.getOntology(),
                factory.getOWLDataPropertyAssertionAxiom(dataProperty, ind, factory.getOWLLiteral(true)));
        realSnapshot.getOntologyManager().applyChange(ax);
    }

    private void addObjectPropertyAssertion(URI property) {
        final OWLObjectProperty objectProperty = factory.getOWLObjectProperty(IRI.create(property));
        final OWLIndividual subject = factory.getOWLAnonymousIndividual();
        final OWLIndividual target = factory.getOWLAnonymousIndividual();
        final AddAxiom ax = new AddAxiom(realSnapshot.getOntology(),
                factory.getOWLObjectPropertyAssertionAxiom(objectProperty, subject, target));
        realSnapshot.getOntologyManager().applyChange(ax);
    }

    @Test
    public void testPersistIndividualWithUnspecifiedPropertyTypePropertiesUnknown() throws Exception {
        initRealOntology();
        final AxiomValueDescriptor descriptor = new AxiomValueDescriptor(NamedResource.create(PK));
        final Assertion opAssertion = Assertion.createPropertyAssertion(
                URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/attributes#hasA"), false);
        descriptor.addAssertion(opAssertion);
        final Set<NamedResource> opValues = new HashSet<>();
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA2"));
        opValues.add(NamedResource.create("http://krizik.felk.cvut.cz/ontologies/jopa/entityA3"));
        opValues.add(NamedResource.create("non-uri-value"));
        opValues.forEach(value -> descriptor.addAssertionValue(opAssertion, new Value<>(value)));

        adapter.persist(descriptor);
        verifyObjectPropertyValuesInOntology(opAssertion, opValues);
        verifyTransactionalChangesRecorded();
    }

    @Test(expected = OWLIndividualExistsException.class)
    public void persistDuplicateIndividualThrowsException() throws Exception {
        final AxiomValueDescriptor descriptorOne = new AxiomValueDescriptor(NamedResource.create(PK));
        descriptorOne.addAssertionValue(Assertion.createClassAssertion(false), new Value<>(URI.create("http://typeA")));
        when(ontologyMock.containsIndividualInSignature(IRI.create(PK))).thenReturn(true);
        final OWLNamedIndividual ind = factory.getOWLNamedIndividual(IRI.create(PK));
        when(ontologyMock.getClassAssertionAxioms(ind))
                .thenReturn(Collections.singleton(factory.getOWLClassAssertionAxiom(
                        factory.getOWLClass(IRI.create("http://typeA")), ind)));
        adapter.persist(descriptorOne);
    }
}
