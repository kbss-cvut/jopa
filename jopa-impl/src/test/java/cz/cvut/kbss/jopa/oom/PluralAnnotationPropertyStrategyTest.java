package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class PluralAnnotationPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();
    private static final NamedResource INDIVIDUAL = NamedResource.create(PK);


    @Mock
    private EntityMappingHelper mapperMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final Configuration configuration = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.LANG, "en"));
        when(mapperMock.getConfiguration()).thenReturn(configuration);

        this.gatherer = new AxiomValueGatherer(INDIVIDUAL, null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassN.class)).thenReturn(mocks.forOwlClassN().entityType());
    }

    @Test
    void createAssertionCreatesAnnotationPropertyAssertion() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final Assertion result = sut.createAssertion();
        assertEquals(Assertion.AssertionType.ANNOTATION_PROPERTY, result.getType());
        assertEquals(Vocabulary.DC_SOURCE, result.getIdentifier().toString());
        assertFalse(result.isInferred());
    }

    private PluralAnnotationPropertyStrategy<OWLClassN> strategyForN() {
        return new PluralAnnotationPropertyStrategy<>(mocks.forOwlClassN().entityType(),
                mocks.forOwlClassN().pluralAnnotationAttribute(), descriptor, mapperMock);
    }

    @Test
    void addValueFromAxiomAddsStringValueToValues() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final String value = "test";
        final Axiom<String> axiom = new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value), instance.getPluralAnnotation());
    }

    private Assertion createAnnotationAssertionForN() {
        return Assertion.createAnnotationPropertyAssertion(URI.create(Vocabulary.DC_SOURCE), false);
    }

    @Test
    void addValueFromAxiomAddsNamedResourceToValues() {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final OWLClassN instance = new OWLClassN();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier().toString()), instance.getPluralAnnotation());
    }

    @Test
    void addValueFromAxiomAddsStringToUriValues() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> sut = createStrategyWithUriAnnotations();
        final NamedResource value = NamedResource.create(Generators.createIndividualIdentifier());
        final Axiom<NamedResource> axiom =
                new AxiomImpl<>(INDIVIDUAL, createAnnotationAssertionForN(), new Value<>(value));
        sut.addValueFromAxiom(axiom);

        final WithPluralUriAnnotations instance = new WithPluralUriAnnotations();
        sut.buildInstanceFieldValue(instance);
        assertEquals(Collections.singleton(value.getIdentifier()), instance.sources);
    }

    private PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> createStrategyWithUriAnnotations() throws
                                                                                                          Exception {
        final EntityTypeImpl<WithPluralUriAnnotations> et = mock(EntityTypeImpl.class);
        final AbstractPluralAttribute<WithPluralUriAnnotations, Set, URI> att = mock(AbstractPluralAttribute.class);
        when(att.getElementType()).thenReturn(BasicTypeImpl.get(URI.class));
        when(att.getCollectionType()).thenReturn(PluralAttribute.CollectionType.SET);
        when(att.getBindableJavaType()).thenReturn(URI.class);
        when(att.getJavaField()).thenReturn(WithPluralUriAnnotations.class.getDeclaredField("sources"));
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.DC_SOURCE));
        return new PluralAnnotationPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE)
    private static class WithPluralUriAnnotations {

        @OWLAnnotationProperty(iri = Vocabulary.DC_SOURCE)
        private Set<URI> sources;
    }

    @Test
    void buildAxiomValuesFromInstanceAddNullValueForNullCollection() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(Collections.singleton(Value.nullValue()));
    }

    private void verifyValuesForNPluralAnnotation(Collection<Value<?>> expected) throws Exception {
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(Collections.singleton(createAnnotationAssertionForN()), valueDescriptor.getAssertions());
        assertEquals(expected.size(), valueDescriptor.getAssertionValues(createAnnotationAssertionForN()).size());
        assertTrue(valueDescriptor.getAssertionValues(createAnnotationAssertionForN()).containsAll(expected));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsNullValueForEmptyCollection() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        instance.setPluralAnnotation(Collections.emptySet());
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(Collections.singleton(Value.nullValue()));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsStringValues() throws Exception {
        final PluralAnnotationPropertyStrategy<OWLClassN> sut = strategyForN();
        final OWLClassN instance = new OWLClassN();
        final Set<String> values = IntStream.range(0, 5).mapToObj(i -> "Value" + i).collect(Collectors.toSet());
        instance.setPluralAnnotation(values);
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(values.stream().map(Value::new).collect(Collectors.toSet()));
    }

    @Test
    void buildAxiomValuesFromInstanceAddsUrisAsNamedResources() throws Exception {
        final PluralAnnotationPropertyStrategy<WithPluralUriAnnotations> sut = createStrategyWithUriAnnotations();
        final Set<URI> values = IntStream.range(0, 5).mapToObj(i -> Generators.createIndividualIdentifier()).collect(
                Collectors.toSet());
        final WithPluralUriAnnotations instance = new WithPluralUriAnnotations();
        instance.sources = values;
        sut.buildAxiomValuesFromInstance(instance, gatherer);

        verifyValuesForNPluralAnnotation(
                values.stream().map(v -> new Value<>(NamedResource.create(v))).collect(Collectors.toSet()));
    }
}