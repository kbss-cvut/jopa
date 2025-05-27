package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.model.EntityManager;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.descriptors.ObjectPropertyCollectionDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.proxy.lazy.gen.LazyLoadingEntityProxy;
import cz.cvut.kbss.jopa.sessions.AbstractUnitOfWork;
import cz.cvut.kbss.jopa.test.OWLClassA;
import cz.cvut.kbss.jopa.test.OWLClassD;
import cz.cvut.kbss.jopa.test.OWLClassDD;
import cz.cvut.kbss.jopa.test.OWLClassFF;
import cz.cvut.kbss.jopa.test.Vocabulary;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
public class ReadOnlyEntityManagerCacheTest extends IntegrationTestBase {
    @Mock
    private Statement statementMock;
    @Mock
    private ResultSet resultSetMock;
    @Mock
    private ResultRow resultRowMock;
    @Mock
    private ResultSetIterator resultSetIteratorMock;

    private EntityManager readOnlyEm;

    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        this.readOnlyEm = emf.createEntityManager(Map.of("cz.cvut.kbss.jopa.transactionMode", "read_only"));
        this.readOnlyEm.getTransaction().begin();
    }

    @AfterEach
    protected void tearDown() {
        this.readOnlyEm.getTransaction().commit();
        this.readOnlyEm.close();
        super.tearDown();
    }

    private boolean isCached(Class<?> cls, Object identifier, Descriptor descriptor) {
        return em.unwrap(AbstractUnitOfWork.class)
                 .getLiveObjectCache()
                 .contains(cls, identifier, descriptor);
    }

    private Object getCachedValue(Class<?> cls, Object identifier, Descriptor descriptor) {
        return em.unwrap(AbstractUnitOfWork.class)
                 .getLiveObjectCache()
                 .get(cls, identifier, descriptor);
    }

    @Test
    void readOnlyEntityManagerDoesNotCacheFindResult() throws Exception {
        final URI instanceUri = Generators.generateUri();
        when(connectionMock.find(any())).thenReturn(axiomsForA(instanceUri));

        final OWLClassA firstA = readOnlyEm.find(OWLClassA.class, instanceUri);
        assertNotNull(firstA);

        // get the original for firstA (the cached entity)
        Descriptor descriptor = new EntityDescriptor();
        assertFalse(isCached(OWLClassA.class, firstA.getUri(), descriptor));

        // connection is invoked for classA (read-only)
        verify(connectionMock).find(any(AxiomDescriptor.class));
    }

    @Test
    void readOnlyEntityManagerDoesNotCacheSingularObjectProperty() throws Exception {
        final URI instanceDUri = Generators.generateUri();
        final URI instanceAUri = Generators.generateUri();
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri
        ))).thenReturn(axiomsForA(instanceAUri));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceDUri
        ))).thenReturn(axiomsForD(instanceDUri, instanceAUri));

        final OWLClassD instanceD = readOnlyEm.find(OWLClassD.class, instanceDUri);
        assertNotNull(instanceD);
        assertNotNull(instanceD.getOwlClassA());

        // entity is not cached
        Descriptor descriptor = new EntityDescriptor();
        assertFalse(isCached(OWLClassA.class, instanceD.getOwlClassA().getUri(), descriptor));

        // connection is invoked for classD (read-only) and for classA (read-only)
        verify(connectionMock, times(2)).find(any(AxiomDescriptor.class));
    }

    @Test
    void readOnlyEntityManagerDoesNotCachePluralObjectProperty() throws Exception {
        final URI instanceFFUri = Generators.generateUri();
        final URI instanceAUri1 = Generators.generateUri();
        final URI instanceAUri2 = Generators.generateUri();
        final URI instanceAUri3 = Generators.generateUri();
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceFFUri
        ))).thenReturn(axiomsForFF(instanceFFUri, instanceAUri1, instanceAUri2, instanceAUri3));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri1
        ))).thenReturn(axiomsForA(instanceAUri1));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri2
        ))).thenReturn(axiomsForA(instanceAUri2));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri3
        ))).thenReturn(axiomsForA(instanceAUri2));

        final OWLClassFF instanceFF = readOnlyEm.find(OWLClassFF.class, instanceFFUri);
        assertNotNull(instanceFF);
        assertNotNull(instanceFF.getSimpleSet());
        assertFalse(instanceFF.getSimpleSet().isEmpty());

        // plural object properties are not cached
        FieldSpecification<?, ?> fs = emf.getMetamodel()
                                         .entity(OWLClassFF.class)
                                         .getFieldSpecification("simpleSet");
        Descriptor descriptor = new ObjectPropertyCollectionDescriptor(fs);
        instanceFF.getSimpleSet().forEach((OWLClassA instance) -> {
            assertFalse(isCached(instance.getClass(), instance.getUri(), descriptor));
        });

        // connection is invoked for classFF (read-only) and three times for classA (read-only)
        verify(connectionMock, times(4)).find(any(AxiomDescriptor.class));
    }

    @Test
    void findResultIsClonedIfItIsAlreadyCached() throws Exception {
        final URI instanceUri = Generators.generateUri();
        when(connectionMock.find(any())).thenReturn(axiomsForA(instanceUri));

        final OWLClassA firstA = em.find(OWLClassA.class, instanceUri);
        assertNotNull(firstA);

        // get the original for firstA (the cached entity)
        Descriptor descriptor = new EntityDescriptor();
        OWLClassA firstAOriginal = (OWLClassA) getCachedValue(OWLClassA.class, instanceUri, descriptor);
        assertNotNull(firstAOriginal);

        final OWLClassA secondA = readOnlyEm.find(OWLClassA.class, instanceUri);
        assertNotNull(secondA);

        // verify secondA is cloned
        assertNotSame(firstAOriginal, secondA);

        // connection is invoked for classA (read-write)
        verify(connectionMock).find(any(AxiomDescriptor.class));
    }

    @Test
    void findResultIsDeepClonedIfItIsAlreadyCached() throws Exception {
        // entire subgraph is cloned if its root is cached
        final URI instanceFFUri = Generators.generateUri();
        final URI instanceAUri1 = Generators.generateUri();
        final URI instanceAUri2 = Generators.generateUri();
        final URI instanceAUri3 = Generators.generateUri();
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceFFUri
        ))).thenReturn(axiomsForFF(instanceFFUri, instanceAUri1, instanceAUri2, instanceAUri3));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri1
        ))).thenReturn(axiomsForA(instanceAUri1));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri2
        ))).thenReturn(axiomsForA(instanceAUri2));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri3
        ))).thenReturn(axiomsForA(instanceAUri2));

        final OWLClassFF instanceFF = em.find(OWLClassFF.class, instanceFFUri);
        OWLClassFF originalFF = (OWLClassFF) getCachedValue(OWLClassFF.class, instanceFFUri, new EntityDescriptor());
        assertNotNull(instanceFF);
        assertNotNull(instanceFF.getSimpleSet());
        assertFalse(instanceFF.getSimpleSet().isEmpty());


        OWLClassFF resultF = readOnlyEm.find(OWLClassFF.class, instanceFFUri);
        assertNotSame(originalFF, resultF);

        FieldSpecification<?, ?> fs = emf.getMetamodel()
                                         .entity(OWLClassFF.class)
                                         .getFieldSpecification("simpleSet");
        Descriptor descriptor = new ObjectPropertyCollectionDescriptor(fs);
        resultF.getSimpleSet().forEach((OWLClassA instance) -> {
            OWLClassA originalA = (OWLClassA) getCachedValue(instance.getClass(), instance.getUri(), descriptor);
            assertNotNull(originalA);
            assertNotSame(originalA, instance);
        });

        // connection is invoked for classFF (read-write) and three times for classA (read-write)
        verify(connectionMock, times(4)).find(any(AxiomDescriptor.class));
    }

    @Test
    void findResultSingularObjectPropertyIsClonedIfAlreadyCached() throws Exception {
        final URI instanceDUri = Generators.generateUri();
        final URI instanceAUri = Generators.generateUri();
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri
        ))).thenReturn(axiomsForA(instanceAUri));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceDUri
        ))).thenReturn(axiomsForD(instanceDUri, instanceAUri));

        final OWLClassA instanceA = em.find(OWLClassA.class, instanceAUri);
        final OWLClassA instanceAOriginal = (OWLClassA) getCachedValue(OWLClassA.class, instanceAUri, new EntityDescriptor());
        assertNotNull(instanceA);
        assertNotNull(instanceAOriginal);

        final OWLClassD instanceD = readOnlyEm.find(OWLClassD.class, instanceDUri);
        assertNotNull(instanceD);
        assertNotNull(instanceD.getOwlClassA());

        // object property is cloned
        assertNotSame(instanceAOriginal, instanceD.getOwlClassA());

        // connection is invoked for classD (read-only) and for classA (read-only)
        verify(connectionMock, times(2)).find(any(AxiomDescriptor.class));
    }

    @Test
    void findResultPluralObjectPropertyIsClonedIfAlreadyCached() throws Exception {
        final URI instanceFFUri = Generators.generateUri();
        final URI instanceAUri1 = Generators.generateUri();
        final URI instanceAUri2 = Generators.generateUri();
        final URI instanceAUri3 = Generators.generateUri();
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceFFUri
        ))).thenReturn(axiomsForFF(instanceFFUri, instanceAUri1, instanceAUri2, instanceAUri3));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri1
        ))).thenReturn(axiomsForA(instanceAUri1));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri2
        ))).thenReturn(axiomsForA(instanceAUri2));
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.getSubject().getIdentifier() == instanceAUri3
        ))).thenReturn(axiomsForA(instanceAUri2));

        final OWLClassA instanceA = em.find(OWLClassA.class, instanceAUri1);
        OWLClassA originalA = (OWLClassA) getCachedValue(OWLClassA.class, instanceAUri1, new EntityDescriptor());
        assertNotNull(originalA);
        assertNotNull(instanceA);

        OWLClassFF resultF = readOnlyEm.find(OWLClassFF.class, instanceFFUri);
        assertNotNull(resultF);

        OWLClassA clone = resultF.getSimpleSet()
                                 .stream()
                                 .filter((OWLClassA a) -> a.getUri() == instanceA.getUri())
                                 .findFirst()
                                 .get();
        assertNotNull(clone);
        assertNotSame(originalA, clone);

        // connection is invoked for classA (read-write), for classFF, and three times for classD (read-write)
        verify(connectionMock, times(5)).find(any(AxiomDescriptor.class));
    }

    @Test
    void queryResultIsClonedWhenItIsAlreadyCached() throws Exception {
        when(connectionMock.createStatement()).thenReturn(statementMock);
        when(resultSetMock.iterator()).thenReturn(resultSetIteratorMock);
        when(resultSetIteratorMock.next()).thenReturn(resultRowMock);
        final URI instanceUri = Generators.generateUri();
        final String query = "SELECT ?x WHERE { ?x a <" + Vocabulary.C_OWL_CLASS_A + "> . }";
        when(statementMock.executeQuery(query)).thenReturn(resultSetMock);
        when(resultSetIteratorMock.hasNext()).thenReturn(true).thenReturn(false);
        when(resultRowMock.isBound(0)).thenReturn(true);
        when(resultRowMock.getString(0)).thenReturn(instanceUri.toString());
        when(connectionMock.find(any())).thenReturn(axiomsForA(instanceUri));

        final OWLClassA firstA = em.find(OWLClassA.class, instanceUri);
        assertNotNull(firstA);

        // get the original for firstA (the cached entity)
        Descriptor descriptor = new EntityDescriptor();
        OWLClassA firstAOriginal = (OWLClassA) getCachedValue(OWLClassA.class, instanceUri, descriptor);

        final OWLClassA secondA = readOnlyEm.createNativeQuery(query, OWLClassA.class).getSingleResult();
        assertNotNull(secondA);

        // secondA should be cloned
        assertNotSame(firstAOriginal, secondA);

        // connection is invoked for classF (read-write)
        verify(connectionMock).find(any(AxiomDescriptor.class));
    }

    @Test
    void loadFieldValueDoesNotCacheObjectProperty() throws Exception {
        final URI instanceDDUri = Generators.generateUri();
        final URI instanceAUri = Generators.generateUri();

        final AxiomDescriptor aDescriptor = new AxiomDescriptor(NamedResource.create(instanceDDUri));
        Assertion assertion = Assertion.createObjectPropertyAssertion(new URI(Vocabulary.P_HAS_OWL_CLASS_A), false);
        aDescriptor.addAssertion(assertion);
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.toString().equals(aDescriptor.toString())
        ))).thenReturn(axiomsForPropertyDDToA(instanceDDUri, instanceAUri));

        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null
                && !axiomDescriptor.toString().equals(aDescriptor.toString())
                && axiomDescriptor.getSubject().getIdentifier() == instanceAUri
        ))).thenReturn(axiomsForA(instanceAUri));

        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null
                && !axiomDescriptor.toString().equals(aDescriptor.toString())
                && axiomDescriptor.getSubject().getIdentifier() == instanceDDUri
        ))).thenReturn(axiomsForDD(instanceDDUri, instanceAUri));

        // load DD by read-only
        // getA should by lazy proxy
        OWLClassDD firstDD = readOnlyEm.find(OWLClassDD.class, instanceDDUri);
        assertNotNull(firstDD);
        assertFalse(isCached(OWLClassDD.class, instanceDDUri, new EntityDescriptor()));
        assertInstanceOf(LazyLoadingEntityProxy.class, firstDD.getOwlClassA());


        // trigger lazy load
        firstDD.getOwlClassA().getStringAttribute();

        // property should be loaded and not cached
        assertInstanceOf(OWLClassA.class, firstDD.getOwlClassA());
        assertFalse(isCached(OWLClassA.class, instanceAUri, new EntityDescriptor()));

        // connection is invoked for classDD, classA (loadFieldValue), and for classA
        verify(connectionMock, times(3)).find(any(AxiomDescriptor.class));
    }

    @Test
    void loadFieldValueMakesCopyOfAlreadyCachedEntity() throws Exception {
        final URI instanceDDUri = Generators.generateUri();
        final URI instanceAUri = Generators.generateUri();

        final AxiomDescriptor aDescriptor = new AxiomDescriptor(NamedResource.create(instanceDDUri));
        Assertion assertion = Assertion.createObjectPropertyAssertion(new URI(Vocabulary.P_HAS_OWL_CLASS_A), false);
        aDescriptor.addAssertion(assertion);
        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null && axiomDescriptor.toString().equals(aDescriptor.toString())
        ))).thenReturn(axiomsForPropertyDDToA(instanceDDUri, instanceAUri));

        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null
                        && !axiomDescriptor.toString().equals(aDescriptor.toString())
                        && axiomDescriptor.getSubject().getIdentifier() == instanceAUri
        ))).thenReturn(axiomsForA(instanceAUri));

        when(connectionMock.find(argThat((AxiomDescriptor axiomDescriptor) ->
                axiomDescriptor != null
                        && !axiomDescriptor.toString().equals(aDescriptor.toString())
                        && axiomDescriptor.getSubject().getIdentifier() == instanceDDUri
        ))).thenReturn(axiomsForDD(instanceDDUri, instanceAUri));

        // load A by read-write
        // value should be cached
        OWLClassA firstA = em.find(OWLClassA.class, instanceAUri);
        OWLClassA firstAOriginal = (OWLClassA) getCachedValue(OWLClassA.class, instanceAUri, new EntityDescriptor());
        assertNotNull(firstA);
        assertNotNull(firstAOriginal);
        assertNotSame(firstAOriginal, firstA);

        // load DD by read-only
        // getA should by lazy proxy
        OWLClassDD firstDD = readOnlyEm.find(OWLClassDD.class, instanceDDUri);
        assertNotNull(firstDD);
        assertInstanceOf(LazyLoadingEntityProxy.class, firstDD.getOwlClassA());
        assertFalse(isCached(OWLClassDD.class, instanceDDUri, new EntityDescriptor()));

        // trigger getA load
        // getA should by cloned
        firstDD.getOwlClassA().getStringAttribute();
        assertInstanceOf(OWLClassA.class, firstDD.getOwlClassA());
        assertNotSame(firstAOriginal, firstDD.getOwlClassA());

        // connection is invoked for classA (read-write) and for classDD (read-only), and for classA (loadFieldValue)
        verify(connectionMock, times(3)).find(any(AxiomDescriptor.class));
    }

    private Collection<Axiom<?>> axiomsForPropertyDDToA(URI identifierDD, URI identifierA) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource nr = NamedResource.create(identifierDD);
        axioms.add(new AxiomImpl<>(nr, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false),
                new Value<Object>(NamedResource.create(identifierA))));
        return axioms;
    }

    private Collection<Axiom<?>> axiomsForA(URI identifier) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource nr = NamedResource.create(identifier);
        axioms.add(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_A))));
        axioms.add(new AxiomImpl<>(nr,
                Assertion.createDataPropertyAssertion(URI.create(Vocabulary.P_A_STRING_ATTRIBUTE),
                        false), new Value<>("stringAttribute")));
        return axioms;
    }

    private Collection<Axiom<?>> axiomsForDD(URI identifierDD, URI identifierA) {
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource nr = NamedResource.create(identifierDD);
        axioms.add(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_D))));
        axioms.add(new AxiomImpl<>(nr, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false),
                new Value<Object>(NamedResource.create(identifierA))));
        return axioms;
    }

    private Collection<Axiom<?>> axiomsForD(URI identifierD, URI identifierA) {
        final NamedResource id = NamedResource.create(identifierD);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        axioms.add(new AxiomImpl<>(id, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_D))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_OWL_CLASS_A), false),
                new Value<Object>(NamedResource.create(identifierA))));
        return axioms;
    }

    private Collection<Axiom<?>> axiomsForFF(URI identifierF, URI identifierA1, URI identifierA2, URI identifierA3) {
        final NamedResource id = NamedResource.create(identifierF);
        final Collection<Axiom<?>> axioms = new ArrayList<>();
        axioms.add(new AxiomImpl<>(id, Assertion.createClassAssertion(false),
                new Value<>(NamedResource.create(Vocabulary.C_OWL_CLASS_F))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), false),
                new Value<Object>(NamedResource.create(identifierA1))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), false),
                new Value<Object>(NamedResource.create(identifierA2))));
        axioms.add(new AxiomImpl<>(id, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_F_HAS_SIMPLE_SET), false),
                new Value<Object>(NamedResource.create(identifierA3))));
        return axioms;
    }
}
