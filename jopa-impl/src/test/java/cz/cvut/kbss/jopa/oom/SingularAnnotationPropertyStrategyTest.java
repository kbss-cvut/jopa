/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.exceptions.IntegrityConstraintViolatedException;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.JOPAPersistenceProperties;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttributeImpl;
import cz.cvut.kbss.jopa.oom.converter.ObjectConverter;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.LangString;
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
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.CoreMatchers.hasItem;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class SingularAnnotationPropertyStrategyTest {

    private static final String LANG = "en";
    private static final URI ID = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private MetamodelMocks mocks;

    private final Descriptor descriptor = new EntityDescriptor();

    @BeforeEach
    void setUp() throws Exception {
        final Configuration configuration = new Configuration(
                Collections.singletonMap(JOPAPersistenceProperties.LANG, LANG));
        when(mapperMock.getConfiguration()).thenReturn(configuration);
        this.mocks = new MetamodelMocks();
    }

    @Test
    void addAxiomValueAddsStringAnnotationValue() {
        final String str = "stringValue";
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final Axiom<String> ax = new AxiomImpl<>(NamedResource.create(ID), annotationForN(), new Value<>(str));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertEquals(str, n.getAnnotationProperty());
    }

    private SingularAnnotationPropertyStrategy<OWLClassN> forN(AbstractAttribute<OWLClassN, ?> att) {
        return new SingularAnnotationPropertyStrategy<>(
                mocks.forOwlClassN().entityType(), att, descriptor, mapperMock);
    }

    private Assertion annotationForN() {
        final URI uri = mocks.forOwlClassN().annotationAttribute().getIRI().toURI();
        return Assertion.createAnnotationPropertyAssertion(uri, LANG, false);
    }

    @Test
    void addAxiomValueSkipsAxiomWhoseValueDoesNotMatchTargetFieldType() {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final Axiom<Integer> ax = new AxiomImpl<>(NamedResource.create(ID), annotationForN(), new Value<>(117));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertNull(n.getAnnotationProperty());
    }

    @Test
    void addAxiomValueAddsPlainIdentifierValueOfAnnotationProperty() {
        final URI value = Generators.createIndividualIdentifier();
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final Axiom<NamedResource> ax = new AxiomImpl<>(NamedResource.create(ID), annotationWithUriForN(),
                                                        new Value<>(NamedResource.create(value)));

        strategy.addValueFromAxiom(ax);
        final OWLClassN n = new OWLClassN();
        strategy.buildInstanceFieldValue(n);
        assertEquals(value, n.getAnnotationUri());
    }

    private Assertion annotationWithUriForN() {
        final URI uri = mocks.forOwlClassN().annotationUriAttribute().getIRI().toURI();
        return Assertion.createAnnotationPropertyAssertion(uri, LANG, false);
    }

    @Test
    void addAxiomValueThrowsIntegrityConstraintsViolationWhenAnotherValueIsAlreadySet() {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final List<Axiom<String>> axioms = new ArrayList<>();
        for (int i = 0; i < 2; i++) {
            axioms.add(new AxiomImpl<>(NamedResource.create(ID), annotationForN(), new Value<>("String" + i)));
        }

        assertThrows(IntegrityConstraintViolatedException.class, () -> axioms.forEach(strategy::addValueFromAxiom));
    }

    @Test
    void buildAxiomsExtractsStringValueOfAnnotationPropertyField() throws Exception {
        final String str = "stringValue";
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(ID.toString());
        n.setAnnotationProperty(str);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationForN());
        assertEquals(str, val.getValue());
    }

    private Value<?> getValueForAssertion(AxiomValueGatherer builder, Assertion assertion) throws Exception {
        final AxiomValueDescriptor desc = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertTrue(desc.getAssertions().contains(assertion));
        assertEquals(1, desc.getAssertionValues(assertion).size());
        return desc.getAssertionValues(assertion).get(0);
    }

    @Test
    void buildAxiomsExtractsPlainIdentifiersAttributeValuesAsNamedResources() throws Exception {
        final URI uri = Generators.createIndividualIdentifier();
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(ID.toString());
        n.setAnnotationUri(uri);
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationWithUriForN());
        assertTrue(val.getValue() instanceof NamedResource);
        assertEquals(uri, ((NamedResource) val.getValue()).getIdentifier());
    }

    @Test
    void buildAxiomsUsesNullValueWhenExtractedFieldValueIsNull() throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(
                mocks.forOwlClassN().annotationUriAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(ID.toString());
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        strategy.buildAxiomValuesFromInstance(n, builder);

        final Value<?> val = getValueForAssertion(builder, annotationWithUriForN());
        assertSame(Value.nullValue(), val);
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToDescriptorLanguage() throws Exception {
        descriptor.setLanguage("cs");
        buildAxiomsAndVerifyLanguageTag("cs");
    }

    private void buildAxiomsAndVerifyLanguageTag(String expectedLang) throws Exception {
        final SingularAnnotationPropertyStrategy<OWLClassN> strategy = forN(mocks.forOwlClassN().annotationAttribute());
        final OWLClassN n = new OWLClassN();
        n.setId(ID.toString());
        n.setAnnotationProperty("english");

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        strategy.buildAxiomValuesFromInstance(n, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final Assertion assertion = valueDescriptor.getAssertions().iterator().next();
        assertTrue(assertion.hasLanguage());
        assertEquals(expectedLang, assertion.getLanguage());
    }

    @Test
    void buildAxiomsSetsLanguageTagAccordingToPUConfigurationWhenItIsNotSpecifiedInDescriptor() throws Exception {
        buildAxiomsAndVerifyLanguageTag(LANG);
    }

    @Test
    void addAxiomValueTransformsValueToLexicalForm() {
        // We are using datatype property field as annotation property field to spare test code
        final SingularAnnotationPropertyStrategy<OWLClassM> sut = new SingularAnnotationPropertyStrategy<>(
                mocks.forOwlClassM().entityType(), mocks.forOwlClassM().lexicalFormAttribute(), descriptor, mapperMock);
        final Integer value = 117;
        final Axiom<Integer> axiom = new AxiomImpl<>(NamedResource.create(ID), lexicalFormAssertion(),
                                                     new Value<>(value));
        sut.addValueFromAxiom(axiom);
        final OWLClassM result = new OWLClassM();
        result.setKey(ID.toString());
        sut.buildInstanceFieldValue(result);
        assertEquals(value.toString(), result.getLexicalForm());
    }

    private Assertion lexicalFormAssertion() {
        return Assertion.createAnnotationPropertyAssertion(URI.create(Vocabulary.p_m_lexicalForm), false);
    }

    @Test
    void addAxiomValueAcceptsNamedResourceForLexicalFormAttribute() {
        // Since annotation properties may reference individuals and String is a valid identifier mapping type, we let JOPA
        // load the value even though it is not a lexical form. Lexical form would apply only to literal values.
        // We are using datatype property field as annotation property field to spare test code
        final SingularAnnotationPropertyStrategy<OWLClassM> sut = new SingularAnnotationPropertyStrategy<>(
                mocks.forOwlClassM().entityType(), mocks.forOwlClassM().lexicalFormAttribute(), descriptor, mapperMock);
        final URI identifier = Generators.createIndividualIdentifier();
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID), lexicalFormAssertion(),
                                                           new Value<>(NamedResource.create(identifier)));
        sut.addValueFromAxiom(axiom);
        final OWLClassM result = new OWLClassM();
        result.setKey(ID.toString());
        sut.buildInstanceFieldValue(result);
        assertEquals(identifier.toString(), result.getLexicalForm());
    }

    @Test
    void addAxiomValueConvertsNamedResourceToUriForAttributeOfTypeObject() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SingularAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final SingularAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new SingularAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final URI identifier = Generators.createIndividualIdentifier();
        final Axiom<NamedResource> axiom = new AxiomImpl<>(NamedResource.create(ID), annotationWithUriForN(),
                                                           new Value<>(NamedResource.create(identifier)));
        sut.addValueFromAxiom(axiom);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        sut.buildInstanceFieldValue(instance);
        assertEquals(identifier, instance.singularAnnotation);
    }

    private SingularAttributeImpl<ClassWithObjectAnnotation, Object> objectAnnotationAttribute(
            EntityType<ClassWithObjectAnnotation> et)
            throws NoSuchFieldException {
        final SingularAttributeImpl<ClassWithObjectAnnotation, Object> att = mock(SingularAttributeImpl.class);
        when(att.getJavaField()).thenReturn(ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation"));
        when(att.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.ANNOTATION);
        when(att.getJavaType()).thenReturn(Object.class);
        when(att.getDeclaringType()).thenReturn(et);
        when(att.getIRI()).thenReturn(IRI.create(Vocabulary.ATTRIBUTE_BASE + "singularAnnotation"));
        when(att.getName())
                .thenReturn(ClassWithObjectAnnotation.class.getDeclaredField("singularAnnotation").getName());
        when(att.getConverter()).thenReturn(new ObjectConverter());
        return att;
    }

    private static class ClassWithObjectAnnotation {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "singularAnnotation")
        private Object singularAnnotation;
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsUriToNamedResourceForAttributeOfTypeObject() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SingularAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final SingularAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new SingularAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        instance.singularAnnotation = Generators.createIndividualIdentifier();

        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        assertEquals(1, values.size());
        assertEquals(NamedResource.create((URI) instance.singularAnnotation), values.get(0).getValue());
    }

    @Test
    void buildAxiomValuesFromInstanceConvertsMultilingualStringTranslationsToLangStringValues() throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SingularAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        final SingularAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new SingularAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        final MultilingualString mls = MultilingualString.create("test", "en");
        mls.set("cs", "test");
        instance.singularAnnotation = mls;
        final AxiomValueGatherer builder = new AxiomValueGatherer(NamedResource.create(ID), null);
        sut.buildAxiomValuesFromInstance(instance, builder);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(builder);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> values = valueDescriptor
                .getAssertionValues(valueDescriptor.getAssertions().iterator().next());
        mls.getValue().forEach((lang, value) -> assertThat(values, hasItem(new Value<>(new LangString(value, lang)))));
    }

    @Test
    void addAxiomValueConvertsLangStringToMultilingualStringWhenTargetIsObjectAndMultilingualStringsArePreferred()
            throws Exception {
        final EntityType<ClassWithObjectAnnotation> et = mock(EntityType.class);
        final SingularAttributeImpl<ClassWithObjectAnnotation, Object> att = objectAnnotationAttribute(et);
        when(att.getConverter()).thenReturn(new ObjectConverter(true));
        final SingularAnnotationPropertyStrategy<ClassWithObjectAnnotation> sut = new SingularAnnotationPropertyStrategy<>(
                et, att, descriptor, mapperMock);
        final LangString langString = new LangString("test", "en");
        final Axiom<LangString> axiom = new AxiomImpl<>(NamedResource.create(ID), annotationWithUriForN(),
                                                        new Value<>(langString));
        sut.addValueFromAxiom(axiom);
        final ClassWithObjectAnnotation instance = new ClassWithObjectAnnotation();
        sut.buildInstanceFieldValue(instance);
        assertEquals(MultilingualString.create(langString.getValue(), langString.getLanguage().get()),
                instance.singularAnnotation);
    }

    @Test
    void isResourceIdentifierTypeReturnsTrueForUriAndUrl() {
        assertTrue(SingularAnnotationPropertyStrategy.isResourceIdentifierType(URI.class));
        assertTrue(SingularAnnotationPropertyStrategy.isResourceIdentifierType(URL.class));
        assertFalse(SingularAnnotationPropertyStrategy.isResourceIdentifierType(String.class));
    }
}
