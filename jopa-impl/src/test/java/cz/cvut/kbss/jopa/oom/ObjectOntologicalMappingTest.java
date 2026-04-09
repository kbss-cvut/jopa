package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.Phone;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.EntityGraph;
import cz.cvut.kbss.jopa.model.EntityGraphImpl;
import cz.cvut.kbss.jopa.model.LoadState;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.Subgraph;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.sessions.AbstractUnitOfWork;
import cz.cvut.kbss.jopa.sessions.cache.CacheManager;
import cz.cvut.kbss.jopa.sessions.descriptor.LoadStateDescriptor;
import cz.cvut.kbss.jopa.sessions.util.AxiomBasedLoadingConfigGroup;
import cz.cvut.kbss.jopa.sessions.util.AxiomBasedLoadingParameters;
import cz.cvut.kbss.jopa.sessions.util.LoadStateDescriptorRegistry;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.allOf;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.hasProperty;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ObjectOntologicalMappingTest {

    @Mock
    private Connection storageConnection;

    @Mock
    private CacheManager cacheManager;

    @Mock
    private AbstractUnitOfWork uow;

    @Mock
    private MetamodelImpl metamodel;

    private ObjectOntologyMapperImpl sut;

    @BeforeEach
    public void setUp() throws Exception {
        final MetamodelMocks metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        when(uow.getMetamodel()).thenReturn(metamodel);
        when(uow.getLiveObjectCache()).thenReturn(cacheManager);
        when(uow.getLoadStateRegistry()).thenReturn(new LoadStateDescriptorRegistry(Object::toString));
        when(uow.getConfiguration()).thenReturn(new Configuration());
        this.sut = new ObjectOntologyMapperImpl(uow, storageConnection);
    }

    @Test
    void loadEntityFromAxiomsLoadsEntityGraphWhenAxiomsForMultipleRelatedEntitiesAreSupplied() {
        final OWLClassD expected = new OWLClassD(Generators.createIndividualIdentifier());
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        expected.setOwlClassA(aInstance);
        final List<Axiom<?>> axioms = instanceAxioms(expected);

        final OWLClassD result = sut.loadEntity(new AxiomBasedLoadingParameters<>(OWLClassD.class, axioms, new AxiomBasedLoadingConfigGroup<>(expected.getUri(), new EntityDescriptor())));
        assertEquals(expected.getUri(), result.getUri());
        assertNotNull(result.getOwlClassA());
        assertThat(result.getOwlClassA(), allOf(
                hasProperty("uri", equalTo(aInstance.getUri())),
                hasProperty("stringAttribute", equalTo(aInstance.getStringAttribute())),
                hasProperty("types", equalTo(aInstance.getTypes()))
        ));
    }

    private static List<Axiom<?>> instanceAxioms(OWLClassD d) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource dIndividual = NamedResource.create(d.getUri());
        axioms.add(new AxiomImpl<>(dIndividual, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_OwlClassD))));
        axioms.add(new AxiomImpl<>(dIndividual, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_h_hasA), false), new Value<>(d.getOwlClassA()
                                                                                                                                              .getUri())));
        final NamedResource aIndividual = NamedResource.create(d.getOwlClassA().getUri());
        axioms.add(new AxiomImpl<>(aIndividual, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_a_stringAttribute), false), new Value<>(d.getOwlClassA()
                                                                                                                                                       .getStringAttribute())));
        axioms.add(new AxiomImpl<>(aIndividual, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_OwlClassA))));
        d.getOwlClassA().getTypes()
         .forEach(type -> axioms.add(new AxiomImpl<>(aIndividual, Assertion.createClassAssertion(false), new Value<>(URI.create(type)))));
        return axioms;
    }

    @Test
    void loadEntityFromAxiomsLoadsEntityWithRelatedEntitiesAndPopulatesLoadStateDescriptorBasedOnFetchGraph() {
        final Person expected = new Person(Generators.createIndividualIdentifier());
        expected.setFirstName("John");
        expected.setLastName("Doe");
        expected.setPhone(new Phone(Generators.createIndividualIdentifier(), "123456789", "Nokia"));
        final List<Axiom<?>> axioms = instanceAxioms(expected);

        final IdentifiableEntityType<Person> et = metamodel.entity(Person.class);
        final EntityGraph<Person> fetchGraph = new EntityGraphImpl<>(et, metamodel);
        // Age is not in the data
        fetchGraph.addAttributeNodes("firstName", "lastName", "age", "phone");
        final Subgraph<Phone> subgraph = fetchGraph.addSubgraph("phone");
        subgraph.addAttributeNodes("number", "brand");
        final Person result = sut.loadEntity(new AxiomBasedLoadingParameters<>(Person.class, axioms, new AxiomBasedLoadingConfigGroup<>(expected.getUri(), new EntityDescriptor(), fetchGraph)));
        assertEquals(expected.getUri(), result.getUri());
        assertEquals(expected.getFirstName(), result.getFirstName());
        assertEquals(expected.getLastName(), result.getLastName());
        assertNull(result.getAge());
        assertNotNull(result.getPhone());
        assertEquals(expected.getPhone().getUri(), result.getPhone().getUri());
        assertEquals(expected.getPhone().getNumber(), result.getPhone().getNumber());
        assertEquals(expected.getPhone().getBrand(), result.getPhone().getBrand());
        final LoadStateDescriptor<Person> personLoadState = uow.getLoadStateRegistry().get(result);
        assertNotNull(personLoadState);
        assertEquals(LoadState.LOADED, personLoadState.isLoaded(et.getAttribute("firstName")));
        assertEquals(LoadState.LOADED, personLoadState.isLoaded(et.getAttribute("lastName")));
        assertEquals(LoadState.LOADED, personLoadState.isLoaded(et.getAttribute("age")));
        assertEquals(LoadState.LOADED, personLoadState.isLoaded(et.getAttribute("phone")));
        assertEquals(LoadState.UNKNOWN, personLoadState.isLoaded(et.getAttribute("username")));
        assertEquals(LoadState.UNKNOWN, personLoadState.isLoaded(et.getAttribute("gender")));
        final LoadStateDescriptor<Phone> phoneLoadState = uow.getLoadStateRegistry().get(result.getPhone());
        assertNotNull(phoneLoadState);
        assertEquals(LoadState.LOADED, phoneLoadState.isLoaded(metamodel.entity(Phone.class).getAttribute("number")));
        assertEquals(LoadState.LOADED, phoneLoadState.isLoaded(metamodel.entity(Phone.class).getAttribute("brand")));
    }

    private static List<Axiom<?>> instanceAxioms(Person person) {
        final List<Axiom<?>> axioms = new ArrayList<>();
        final NamedResource pIndividual = NamedResource.create(person.getUri());
        axioms.add(new AxiomImpl<>(pIndividual, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_Person))));
        axioms.add(new AxiomImpl<>(pIndividual, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_p_firstName), false), new Value<>(person.getFirstName())));
        axioms.add(new AxiomImpl<>(pIndividual, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_p_lastName), false), new Value<>(person.getLastName())));
        final NamedResource phoneIndividual = NamedResource.create(person.getPhone().getUri());
        axioms.add(new AxiomImpl<>(pIndividual, Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_p_hasPhone), false), new Value<>(person.getPhone()
                                                                                                                                                       .getUri())));
        axioms.add(new AxiomImpl<>(phoneIndividual, Assertion.createClassAssertion(false), new Value<>(URI.create(Vocabulary.c_Phone))));
        axioms.add(new AxiomImpl<>(phoneIndividual, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_p_phoneNumber), false), new Value<>(person.getPhone()
                                                                                                                                                            .getNumber())));
        axioms.add(new AxiomImpl<>(phoneIndividual, Assertion.createDataPropertyAssertion(URI.create(Vocabulary.p_p_phoneBrand), false), new Value<>(person.getPhone()
                                                                                                                                                           .getBrand())));
        return axioms;
    }
}
