/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class EntityDescriptorTest {

    private static final URI CONTEXT_ONE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextOne");
    private static final URI CONTEXT_TWO = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/contextTwo");
    private static final String LANG = "en";

    @Mock
    private Attribute<TestClass, String> stringAtt;

    @BeforeEach
    void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(stringAtt.getJavaField()).thenReturn(TestClass.stringAttField());
    }

    @Test
    void fieldDescriptorByDefaultInheritsEntityContext() {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertEquals(CONTEXT_ONE, result.getContext());
    }

    @Test
    void fieldDescriptorHasItsOwnContextWhenItIsSetForIt() {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeContext(stringAtt.getJavaField(), CONTEXT_TWO);

        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertEquals(CONTEXT_TWO, result.getContext());
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
        descriptor.setAttributeLanguage(stringAtt.getJavaField(), newLang);

        final Descriptor result = descriptor.getAttributeDescriptor(stringAtt);
        assertTrue(result.hasLanguage());
        assertEquals(newLang, result.getLanguage());
    }

    @Test
    void twoEntityDescriptorsAreEqualWhenTheirFieldDescriptorsHaveTheSameContexts() throws Exception {
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final EntityDescriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dTwo.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dOne.addAttributeDescriptor(TestClass.intAttField(), new FieldDescriptor(CONTEXT_ONE, TestClass.intAttField()));
        dTwo.addAttributeDescriptor(TestClass.intAttField(), new FieldDescriptor(CONTEXT_ONE, TestClass.intAttField()));

        assertEquals(dOne, dTwo);
        assertEquals(dTwo, dOne);
        assertEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoEntityDescriptorsAreNotEqualWhenTheyDifferInFieldContext() throws Exception {
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final EntityDescriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dTwo.addAttributeContext(TestClass.stringAttField(), CONTEXT_ONE);

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
        final Descriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        final Descriptor dTwo = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dTwo.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dOne.setLanguage("en");
        dTwo.setLanguage("cs");
        assertNotEquals(dOne, dTwo);
        assertNotEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithDifferentAttributeLanguageTagsAreNotEqual() throws Exception {
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dTwo.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dOne.setLanguage("en");
        dTwo.setLanguage("en");
        dOne.setAttributeLanguage(TestClass.stringAttField(), "en");
        dTwo.setAttributeLanguage(TestClass.stringAttField(), "cs");
        assertNotEquals(dOne, dTwo);
        assertNotEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithSameAttributeLanguageTagsAreEqual() throws Exception {
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.setAttributeLanguage(TestClass.stringAttField(), "en");
        dTwo.setAttributeLanguage(TestClass.stringAttField(), "en");
        assertEquals(dOne, dTwo);
        assertEquals(dOne.hashCode(), dTwo.hashCode());
    }

    @Test
    void twoDescriptorsWithNullLanguageTagSetAreEqual() throws Exception {
        final Descriptor dOne = new EntityDescriptor();
        final Descriptor dTwo = new EntityDescriptor();
        dOne.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
        dTwo.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);
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
    void addAttributeDescriptorProvidesCorrectContextOnRetrieval() throws Exception {
        final EntityDescriptor dOne = new EntityDescriptor(CONTEXT_ONE);
        dOne.addAttributeDescriptor(TestClass.intAttField(), new FieldDescriptor(CONTEXT_TWO, TestClass.intAttField()));
        assertFalse(dOne.getAttributeDescriptors().isEmpty());
        assertEquals(CONTEXT_TWO, dOne.getAttributeDescriptors().iterator().next().getContext());
    }

    @Test
    void equalsHandlesRecursiveDescriptors() throws Exception {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"), descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"), descriptorTwo);
        assertEquals(descriptor, descriptorTwo);
    }

    @SuppressWarnings("unused")
    private static class RecursiveClass {

        private RecursiveClass parent;
    }

    @Test
    void equalsHandlesOneRecursiveDescriptorAndOtherNormal() throws Exception {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"), descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"),
                new EntityDescriptor(CONTEXT_ONE));
        // The first descriptor is recursive, while the second isn't, so it has only two levels
        assertNotEquals(descriptor, descriptorTwo);
    }

    @Test
    void equalsReturnsFalseWhenFieldDescriptorCountDiffers() throws Exception {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.setLanguage("en");
        descriptor.setAttributeLanguage(TestClass.stringAttField(), "cs");

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptor.setLanguage("en");
        assertNotEquals(descriptor, descriptorTwo);
    }

    @Test
    void hashCodeHandlesRecursiveDescriptors() throws Exception {
        final EntityDescriptor descriptor = new EntityDescriptor(CONTEXT_ONE);
        descriptor.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"), descriptor);

        final EntityDescriptor descriptorTwo = new EntityDescriptor(CONTEXT_ONE);
        descriptorTwo.addAttributeDescriptor(RecursiveClass.class.getDeclaredField("parent"), descriptorTwo);

        assertEquals(descriptor.hashCode(), descriptorTwo.hashCode());
    }

    @Test
    void getAttributeContextReturnsSubjectContextWhenAssertionsInSubjectContextIsTrue() throws Exception {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE);
        sut.addAttributeDescriptor(TestClass.stringAttField(), new EntityDescriptor(CONTEXT_TWO));

        assertEquals(CONTEXT_ONE, sut.getAttributeContext(stringAtt));
    }

    @Test
    void getAttributeContextReturnsAttributeContextWhenAssertionsInSubjectContextIsFalse() throws Exception {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE, false);
        sut.addAttributeDescriptor(TestClass.stringAttField(), new EntityDescriptor(CONTEXT_TWO));

        assertEquals(CONTEXT_TWO, sut.getAttributeContext(stringAtt));
    }

    @Test
    void getAttributeContextReturnsAttributeContextWhenItOverridesSubjectContext() throws Exception {
        final EntityDescriptor sut = new EntityDescriptor(CONTEXT_ONE, true);
        sut.addAttributeContext(TestClass.stringAttField(), CONTEXT_TWO);

        assertEquals(CONTEXT_TWO, sut.getAttributeContext(stringAtt));
    }
}