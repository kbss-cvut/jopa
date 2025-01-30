/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.Inferred;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.CollectionType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.oom.converter.ObjectOneOfEnumConverter;
import cz.cvut.kbss.jopa.vocabulary.OWL;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
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

import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SimpleSetPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    @Mock
    private ReferenceSavingResolver referenceResolverMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;
    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
        this.mocks = new MetamodelMocks();
        when(mapperMock.getEntityType(OWLClassA.class)).thenReturn(mocks.forOwlClassA().entityType());
    }

    @Test
    void extractsValuesFromInstance() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(generateSet(true));
        when(referenceResolverMock.shouldSaveReferenceToItem(any(), anySet())).thenReturn(true);
        strategy.buildAxiomValuesFromInstance(j, gatherer);
        final Set<URI> expected = j.getOwlClassA().stream().map(OWLClassA::getUri).collect(Collectors.toSet());
        verifyExtractedValues(expected);
    }

    private <T, E> SimpleSetPropertyStrategy<T> strategy(EntityType<T> et, AbstractPluralAttribute<T, Set<E>, E> att) {
        return new SimpleSetPropertyStrategy<>(et, att, descriptor, mapperMock);
    }

    private void verifyExtractedValues(Set<URI> expected) throws Exception {
        final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(NamedResource.create(PK), res.getSubject());
        final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
                OWLObjectProperty.class);
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
                                                                      OWLClassJ.getOwlClassAField()
                                                                               .getAnnotation(Inferred.class) != null);
        assertEquals(expected.size(), res.getAssertionValues(ass).size());
        for (URI u : expected) {
            assertTrue(res.getAssertionValues(ass).contains(new Value<>(NamedResource.create(u))));
        }
    }

    @Test
    void extractValuesSkipsNullItem() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(generateSet(true));
        j.getOwlClassA().add(null);
        when(referenceResolverMock.shouldSaveReferenceToItem(any(), anySet())).thenReturn(true);

        strategy.buildAxiomValuesFromInstance(j, gatherer);
        final Set<URI> expected = j.getOwlClassA().stream().filter(Objects::nonNull).map(OWLClassA::getUri)
                                   .collect(Collectors.toSet());
        verifyExtractedValues(expected);
    }

    private Set<OWLClassA> generateSet(boolean withUris) {
        final Set<OWLClassA> set = new HashSet<>();
        for (int i = 0; i < 10; i++) {
            final OWLClassA a = new OWLClassA();
            if (withUris) {
                a.setUri(Generators.createIndividualIdentifier());
            }
            set.add(a);
        }
        return set;
    }

    @Test
    void extractValuesFromInstanceWhenSetIsNullCreatesNullValueAxiom() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(null);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(PK), null);
        strategy.buildAxiomValuesFromInstance(j, builder);
        final AxiomValueDescriptor res = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(NamedResource.create(PK), res.getSubject());
        final OWLObjectProperty op = OWLClassJ.getOwlClassAField().getAnnotation(
                OWLObjectProperty.class);
        final Assertion ass = Assertion.createObjectPropertyAssertion(URI.create(op.iri()),
                                                                      OWLClassJ.getOwlClassAField()
                                                                               .getAnnotation(Inferred.class) != null);
        assertEquals(1, res.getAssertionValues(ass).size());
        assertSame(Value.nullValue(), res.getAssertionValues(ass).get(0));
    }

    private Collection<Axiom<NamedResource>> buildAxiomsForSet(URI property, Set<OWLClassA> set) {
        final NamedResource subject = NamedResource.create(PK);
        final Assertion assertion = Assertion.createObjectPropertyAssertion(property, false);
        return set.stream().map(a -> new AxiomImpl<>(subject, assertion, new Value<>(NamedResource.create(a.getUri()))))
                  .collect(Collectors.toList());
    }

    @Test
    void buildsInstanceFieldAsSetOfUrls() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().pUrlsAttribute());
        final URI property =
                URI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri());
        final Set<OWLClassA> values = generateSet(true);
        final Collection<Axiom<NamedResource>> axioms = buildAxiomsForSet(property, values);
        axioms.forEach(strategy::addAxiomValue);

        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        assertNotNull(p.getIndividualUrls());
        assertEquals(values.size(), p.getIndividualUrls().size());
        values.forEach(a -> {
            try {
                assertTrue(p.getIndividualUrls().contains(a.getUri().toURL()));
            } catch (MalformedURLException e) {
                throw new RuntimeException("Failed to assert test result.", e);
            }
        });
    }

    @Test
    void extractsValuesFromFieldWithSetOfPlainIdentifiers() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().pUrlsAttribute());
        final OWLClassP p = new OWLClassP();
        strategy.setReferenceSavingResolver(referenceResolverMock);
        p.setUri(PK);
        setIndividualUrls(p);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        verifyExtractedValuesForP(p.getIndividualUrls());
    }

    private void verifyExtractedValuesForP(Set<URL> expected) throws Exception {
        final AxiomValueDescriptor axiomDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        final URI property =
                URI.create(OWLClassP.getIndividualUrlsField().getAnnotation(OWLObjectProperty.class).iri());
        final List<Value<?>>
                values = axiomDescriptor.getAssertionValues(Assertion.createObjectPropertyAssertion(property, false));
        assertEquals(expected.size(), values.size());
        for (Value<?> v : values) {
            assertInstanceOf(NamedResource.class, v.getValue());
            assertTrue(expected.contains(new URL(v.stringValue())));
        }
    }

    private void setIndividualUrls(OWLClassP p) {
        final Set<OWLClassA> aSet = generateSet(true);
        p.setIndividualUrls(aSet.stream().map(a -> {
            try {
                return a.getUri().toURL();
            } catch (MalformedURLException e) {
                throw new IllegalStateException(e);
            }
        }).collect(Collectors.toSet()));
    }

    @Test
    void extractValuesSkipsNullPlainIdentifiers() throws Exception {
        final SimpleSetPropertyStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().pUrlsAttribute());
        final OWLClassP p = new OWLClassP();
        strategy.setReferenceSavingResolver(referenceResolverMock);
        p.setUri(PK);
        setIndividualUrls(p);
        p.getIndividualUrls().add(null);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final Set<URL> expected = p.getIndividualUrls().stream().filter(Objects::nonNull).collect(Collectors.toSet());
        // Added null should be skipped and the original set of values prepared for save
        verifyExtractedValuesForP(expected);
    }

    @Test
    void extractValuesRegistersPendingChangesForInstancesWithNullIdentifier() {
        final SimpleSetPropertyStrategy<OWLClassJ> strategy =
                strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        strategy.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassJ j = new OWLClassJ(PK);
        j.setOwlClassA(generateSet(false));
        strategy.buildAxiomValuesFromInstance(j, gatherer);

        final NamedResource subject = NamedResource.create(PK);
        j.getOwlClassA().forEach(a -> verify(
                referenceResolverMock).registerPendingReference(subject, strategy.createAssertion(), a, null));
    }

    @Test
    void extractValuesConvertsEnumConstantsToNamedResourcesForEnumValuedObjectProperty() throws Exception {
        final EntityType<PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum> et =
                mock(EntityType.class);
        final AbstractPluralAttribute<PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum, Set<OneOfEnum>, OneOfEnum>
                att = mock(AbstractPluralAttribute.class);
        when(att.getBindableJavaType()).thenReturn(OneOfEnum.class);
        when(att.getJavaField()).thenReturn(
                PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum.class.getDeclaredField("enumSet"));
        when(att.getConverter()).thenReturn(new ObjectOneOfEnumConverter<>(OneOfEnum.class));
        when(att.getCollectionType()).thenReturn(CollectionType.SET);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.p_m_objectOneOfEnumAttribute));
        final SimpleSetPropertyStrategy<PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum> sut =
                strategy(et, att);
        sut.setReferenceSavingResolver(referenceResolverMock);
        when(referenceResolverMock.shouldSaveReference(any(OneOfEnum.class), anySet())).thenReturn(true);

        final PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum instance =
                new PluralObjectPropertyStrategyTest.EntityWithPluralObjectPropertyEnum();
        instance.uri = PK;
        instance.enumSet = new HashSet<>(Arrays.asList(OneOfEnum.ANNOTATION_PROPERTY, OneOfEnum.DATATYPE_PROPERTY));

        sut.buildAxiomValuesFromInstance(instance, gatherer);
        final AxiomValueDescriptor axiomDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        final List<Value<?>> values = axiomDescriptor.getAssertionValues(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.p_m_objectOneOfEnumAttribute), false));
        assertEquals(instance.enumSet.size(), values.size());
        assertThat(values, hasItems(new Value<>(NamedResource.create(OWL.DATATYPE_PROPERTY)),
                                    new Value<>(NamedResource.create(OWL.ANNOTATION_PROPERTY))));
    }

    @Test
    void buildAxiomValuesFromInstanceSkipsInferredValues() throws Exception{
        when(mocks.forOwlClassJ().setAttribute().isInferred()).thenReturn(true);
        final SimpleSetPropertyStrategy<OWLClassJ> sut =
                strategy(mocks.forOwlClassJ().entityType(), mocks.forOwlClassJ().setAttribute());
        sut.setReferenceSavingResolver(referenceResolverMock);
        final OWLClassJ instance = new OWLClassJ(PK);
        final List<OWLClassA> aList = Arrays.asList(Generators.generateOwlClassAInstance(), Generators.generateOwlClassAInstance());
        instance.setOwlClassA(new HashSet<>(aList));
        when(referenceResolverMock.shouldSaveReferenceToItem(any(), anySet())).thenReturn(true);
        doAnswer(args -> {
            final Axiom<?> ax = args.getArgument(0, Axiom.class);
            return ax.getValue().stringValue().equals(aList.get(0).getUri().toString());
        }).when(mapperMock).isInferred(any(Axiom.class), any());

        sut.buildAxiomValuesFromInstance(instance, gatherer);
        final AxiomValueDescriptor axiomDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        final List<Value<?>> values = axiomDescriptor.getAssertionValues(
                Assertion.createObjectPropertyAssertion(URI.create(Vocabulary.P_HAS_A), true));
        assertThat(values, hasItem(new Value<>(NamedResource.create(aList.get(1).getUri()))));
        assertThat(values, not(hasItem(new Value<>(NamedResource.create(aList.get(0).getUri())))));
    }
}
