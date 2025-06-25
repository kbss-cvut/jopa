/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.exceptions.AmbiguousContextException;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.PluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.net.URI;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class EntityDescriptorTest {

    private static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextTwo");
    private static final String LANG = "en";

    @Mock
    private Attribute<TestClass, String> stringAtt;

    @Mock
    private Attribute<TestClass, Integer> intAtt;

    @Mock
    private Attribute<RecursiveClass, RecursiveClass> parentAtt;

    @Test
    void fieldDescriptorByDefaultInheritsEntityContext() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertEquals(Collections.singleton(CONTEXT_ONE), result.getContexts());
    }

    @Test
    void fieldDescriptorHasItsOwnContextWhenItIsSetForIt() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeContext(stringAtt, CONTEXT_TWO);

        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertEquals(Collections.singleton(CONTEXT_TWO), result.getContexts());
    }

    @Test
    void fieldDescriptorByDefaultInheritsEntityLanguageTag() {
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage(LANG);
        assertTrue(descriptor.hasLanguage());

        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertTrue(result.hasLanguage());
        assertEquals(LANG, result.getLanguage());
    }

    @Test
    void fieldDescriptorInheritsChangeOfLanguageTagFromEntityDescriptor() {
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage(LANG);
        final String newLang = "cs";
        descriptor.setLanguage(newLang);
        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertTrue(result.hasLanguage());
        assertEquals(newLang, result.getLanguage());
    }

    @Test
    void fieldDescriptorHasLanguageSetToItThroughEntityDescriptor() {
        final EntityDescriptor descriptor = new EntityDescriptor();
        descriptor.setLanguage(LANG);
        final String newLang = "cs";
        descriptor.setAttributeLanguage(stringAtt, newLang);

        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertTrue(result.hasLanguage());
        assertEquals(newLang, result.getLanguage());
    }

    @Test
    void twoEntityDescriptorsAreEqualWhenTheirFieldDescriptorsHaveTheSameContexts() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        when(intAtt.getJavaField()).thenReturn(TestClass.intAttField());
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final EntityDescriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(stringAtt, CONTEXT_TWO);
        dTwo.addAttributeContext(stringAtt, CONTEXT_TWO);
        dOne.addAttributeDescriptor(intAtt, new FieldDescriptor(CONTEXT_ONE, intAtt));
        dTwo.addAttributeDescriptor(intAtt, new FieldDescriptor(CONTEXT_ONE, intAtt));

        assertEquals(dOne, dTwo);
        assertEquals(dTwo, dOne);
        assertEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoEntityDescriptorsAreNotEqualWhenTheyDifferInFieldContext() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final EntityDescriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(stringAtt, CONTEXT_TWO);
        dTwo.addAttributeContext(stringAtt, CONTEXT_ONE);

        assertNotEquals(dOne, dTwo);
        assertNotEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void hasLanguageReturnsTrueForLanguageSetExplicitlyToNull() {
        final Descriptor descriptor = new EntityDescriptor();
        assertFalse(descriptor.hasLanguage());
        descriptor.setLanguage(null);
        assertTrue(descriptor.hasLanguage());
        assertNull(descriptor.getLanguage());
    }

    @Test
    void gettingFieldDescriptorFromEntityDescriptorLeavesItsHasLanguageStatusEmpty() {
        final Descriptor descriptor = new EntityDescriptor();
        final Descriptor fieldDescriptor = descriptor.getAttributeDescriptor(stringAtt);
        assertFalse(fieldDescriptor.hasLanguage());
    }

    @Test
    void twoDescriptorsWithDifferentLanguageTagsAreNotEqual() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final Descriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(stringAtt, CONTEXT_TWO);
        dTwo.addAttributeContext(stringAtt, CONTEXT_TWO);
        dOne.setLanguage("en");
        dTwo.setLanguage("cs");
        assertNotEquals(dOne, dTwo);
        assertNotEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithDifferentAttributeLanguageTagsAreNotEqual() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.addAttributeContext(stringAtt, CONTEXT_TWO);
        dTwo.addAttributeContext(stringAtt, CONTEXT_TWO);
        dOne.setLanguage("en");
        dTwo.setLanguage("en");
        dOne.setAttributeLanguage(stringAtt, "en");
        dTwo.setAttributeLanguage(stringAtt, "cs");
        assertNotEquals(dOne, dTwo);
        assertNotEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithSameAttributeLanguageTagsAreEqual() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.setAttributeLanguage(stringAtt, "en");
        dTwo.setAttributeLanguage(stringAtt, "en");
        assertEquals(dOne, dTwo);
        assertEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithNullLanguageTagSetAreEqual() throws Exception {
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
        when(stringAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.DATA);
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.addAttributeContext(stringAtt, CONTEXT_TWO);
        dTwo.addAttributeContext(stringAtt, CONTEXT_TWO);
        dOne.setLanguage(null);
        dTwo.setLanguage(null);
        assertEquals(dOne, dTwo);
        assertEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void anyLanguageSetsLanguageTagToSupportAny() {
        final Descriptor descriptor = new EntityDescriptor();
        assertFalse(descriptor.hasLanguage());
        descriptor.anyLanguage();
        assertTrue(descriptor.hasLanguage());
        assertNull(descriptor.getLanguage());
    }

    @Test
    void addAttributeDescriptorProvidesCorrectContextOnRetrieval() {
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeDescriptor(intAtt, new FieldDescriptor(CONTEXT_TWO, intAtt));
        assertFalse(dOne.getAttributeDescriptors().isEmpty());
        assertEquals(Collections.singleton(CONTEXT_TWO),
                dOne.getAttributeDescriptors().iterator().next().getContexts());
    }

    @Test
    void equalsHandlesRecursiveDescriptors() {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(parentAtt, descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(parentAtt, descriptorTwo);
        assertEquals(descriptor, descriptorTwo);
    }

    @SuppressWarnings("unused")
    private static class RecursiveClass {

        private RecursiveClass parent;
    }

    @Test
    void equalsHandlesOneRecursiveDescriptorAndOtherNormal() {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(parentAtt, descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(parentAtt, new EntityDescriptor(CONTEXT_ONE));
        // The first descriptor is recursive, while the second isn't, so it has only two levels
        assertNotEquals(descriptor, descriptorTwo);
    }

    @Test
    void equalsReturnsFalseWhenFieldDescriptorCountDiffers() {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.setLanguage("en");
        descriptor.setAttributeLanguage(stringAtt, "cs");

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptor.setLanguage("en");
        assertNotEquals(descriptor, descriptorTwo);
    }

    @Test
    void hashCodeHandlesRecursiveDescriptors() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(parentAtt, descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(parentAtt, descriptorTwo);

        assertEquals(descriptor.hashCode(), descriptorTwo.hashCode());
    }

    @Test
    void getAttributeContextReturnsSubjectContextWhenAssertionsInSubjectContextIsTrue() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addAttributeDescriptor(stringAtt, new EntityDescriptor(CONTEXT_TWO));

        assertEquals(Collections.singleton(CONTEXT_ONE), sut.getAttributeContexts(stringAtt));
    }

    @Test
    void getAttributeContextReturnsAttributeContextWhenAssertionsInSubjectContextIsFalse() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE, false);
        sut.addAttributeDescriptor(stringAtt, new EntityDescriptor(CONTEXT_TWO));

        assertEquals(Collections.singleton(CONTEXT_TWO), sut.getAttributeContexts(stringAtt));
    }

    @Test
    void getAttributeContextReturnsAttributeContextWhenItOverridesSubjectContext() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE, true);
        sut.addAttributeContext(stringAtt, CONTEXT_TWO);

        assertEquals(Collections.singleton(CONTEXT_TWO), sut.getAttributeContexts(stringAtt));
    }

    @Test
    void addAttributeContextCreatesEntityDescriptorAndAddsContextToItWhenItDidNotExist() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        when(parentAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(parentAtt.getJavaType()).thenReturn(RecursiveClass.class);
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeContext(parentAtt, CONTEXT_ONE);

        assertThat(sut.getAttributeContexts(parentAtt), empty());
        final Descriptor result = sut.getAttributeDescriptor(parentAtt);
        assertEquals(Collections.singleton(CONTEXT_ONE), result.getContexts());
    }

    @Test
    void addAttributeContextCreatesFieldDescriptorAndSetsItsContextForDataPropertyField() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addAttributeContext(stringAtt, CONTEXT_TWO);

        assertEquals(Collections.singleton(CONTEXT_TWO), sut.getAttributeContexts(stringAtt));
        final Descriptor result = sut.getAttributeDescriptor(stringAtt);
        assertEquals(Collections.singleton(CONTEXT_TWO), result.getContexts());
    }

    @Test
    void addAttributeContextSupportsMultipleContextsAddedToObjectPropertyAttribute() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        when(parentAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(parentAtt.getJavaType()).thenReturn(RecursiveClass.class);
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeContext(parentAtt, CONTEXT_ONE).addAttributeContext(parentAtt, CONTEXT_TWO);

        assertThat(sut.getAttributeContexts(parentAtt), empty());
        final Descriptor result = sut.getAttributeDescriptor(parentAtt);
        assertEquals(2, result.getContexts().size());
        assertThat(result.getContexts(), hasItems(CONTEXT_ONE, CONTEXT_TWO));
    }

    @Test
    void addAttributeContextSupportsMultipleContextsAddedToDataPropertyAttribute() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addAttributeContext(stringAtt, CONTEXT_ONE).addAttributeContext(stringAtt, CONTEXT_TWO);

        assertEquals(2, sut.getAttributeContexts(stringAtt).size());
        assertThat(sut.getAttributeContexts(stringAtt), hasItems(CONTEXT_ONE, CONTEXT_TWO));
        final Descriptor result = sut.getAttributeDescriptor(stringAtt);
        assertEquals(2, result.getContexts().size());
        assertThat(result.getContexts(), hasItems(CONTEXT_ONE, CONTEXT_TWO));
    }

    @Test
    void addAttributeContextSupportsCreatesObjectPropertyCollectionDescriptorForPluralObjectProperty() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        when(parentAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(parentAtt.isCollection()).thenReturn(true);    // Abusing existing field specification
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeContext(parentAtt, CONTEXT_ONE);

        assertThat(sut.getAttributeContexts(parentAtt), empty());
        final Descriptor result = sut.getAttributeDescriptor(parentAtt);
        assertEquals(Collections.singleton(CONTEXT_ONE), result.getContexts());
    }

    @Test
    void getSingleContextReturnsOneContextSpecifiedInDescriptor() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        assertTrue(sut.getSingleContext().isPresent());
        assertEquals(CONTEXT_ONE, sut.getSingleContext().get());
    }

    @Test
    void getSingleContextReturnsEmptyOptionalWhenNoContextsAreSetInDescriptor() {
        final EntityDescriptor sut = new EntityDescriptor();
        assertFalse(sut.getSingleContext().isPresent());
    }

    @Test
    void getSingleContextThrowsAmbiguousContextExceptionWhenMultipleContextsAreSpecifiedInDescriptor() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addContext(CONTEXT_TWO);
        assertThrows(AmbiguousContextException.class, sut::getSingleContext);
    }

    @Test
    void getSingleAttributeContextReturnsOneContextSpecifiedForAttribute() {
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeContext(stringAtt, CONTEXT_ONE);
        assertTrue(sut.getSingleAttributeContext(stringAtt).isPresent());
        assertEquals(CONTEXT_ONE, sut.getSingleAttributeContext(stringAtt).get());
    }

    @Test
    void getSingleAttributeContextReturnsEmptyOptionalWhenDefaultContextIsSetForAttribute() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addAttributeContext(stringAtt, null);
        assertFalse(sut.getSingleAttributeContext(stringAtt).isPresent());
    }

    @Test
    void getSingleAttributeContextReturnsDescriptorContextWhenNoAttributeContextIsSpecified() {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        assertTrue(sut.getSingleAttributeContext(stringAtt).isPresent());
        assertEquals(CONTEXT_ONE, sut.getSingleAttributeContext(stringAtt).get());
    }

    @Test
    void getSingleAttributeContextThrowsAmbiguousContextExceptionWhenMultipleAttributeContextsAreSpecifiedInDescriptor() {
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeContext(stringAtt, CONTEXT_ONE);
        sut.addAttributeContext(stringAtt, CONTEXT_TWO);
        assertThrows(AmbiguousContextException.class, () -> sut.getSingleAttributeContext(stringAtt));
    }

    @Test
    void includeInferredIsByDefaultInheritedFromParentEntityDescriptor() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        when(parentAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(parentAtt.getJavaType()).thenReturn(RecursiveClass.class);
        final EntityDescriptor sut = new EntityDescriptor();
        sut.disableInference();
        assertFalse(sut.includeInferred());
        final Descriptor attDesc = sut.getAttributeDescriptor(parentAtt);
        assertFalse(attDesc.includeInferred());
    }

    @Test
    void includeInferredInheritedFromParentDescriptorCanBeOverriddenInChildEntityDescriptor() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        when(parentAtt.getPersistentAttributeType()).thenReturn(Attribute.PersistentAttributeType.OBJECT);
        when(parentAtt.getJavaType()).thenReturn(RecursiveClass.class);
        final EntityDescriptor sut = new EntityDescriptor();
        sut.disableInference();
        assertFalse(sut.includeInferred());
        final Descriptor attDesc = sut.getAttributeDescriptor(parentAtt);
        attDesc.enableInference();
        assertTrue(attDesc.includeInferred());
    }

    @Test
    void addAttributeDescriptorWrapsEntityDescriptorInObjectPropertyCollectionDescriptorWhenAttributeIsObjectPropertyCollection() {
        final PluralAttribute pluralAtt = mock(PluralAttribute.class);
        final Type et = mock(Type.class);
        when(pluralAtt.getElementType()).thenReturn(et);
        when(et.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        final EntityDescriptor sut = new EntityDescriptor();
        final EntityDescriptor elementDescriptor = new EntityDescriptor(CONTEXT_ONE);

        sut.addAttributeDescriptor(pluralAtt, elementDescriptor);
        final Descriptor result = sut.getAttributeDescriptor(pluralAtt);
        assertInstanceOf(ObjectPropertyCollectionDescriptor.class, result);
        assertEquals(elementDescriptor, result.unwrap());
    }

    @Test
    void equalsHandlesRecursiveReferences() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeDescriptor(parentAtt, sut);

        final EntityDescriptor other = new EntityDescriptor();
        other.addAttributeDescriptor(parentAtt, other);

        assertEquals(sut, other);
    }

    @Test
    void hashCodeHandlesRecursiveReferences() throws Exception {
        when(parentAtt.getJavaField()).thenReturn(RecursiveClass.class.getDeclaredField("parent"));
        final EntityDescriptor sut = new EntityDescriptor();
        sut.addAttributeDescriptor(parentAtt, sut);

        final EntityDescriptor other = new EntityDescriptor();
        other.addAttributeDescriptor(parentAtt, other);

        assertEquals(sut.hashCode(), other.hashCode());
    }
}
