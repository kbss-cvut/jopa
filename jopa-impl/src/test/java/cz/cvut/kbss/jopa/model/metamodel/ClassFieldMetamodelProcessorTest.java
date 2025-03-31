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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassV;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraints;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import cz.cvut.kbss.jopa.vocabulary.RDF;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ClassFieldMetamodelProcessorTest {

    private final MetamodelBuilder metamodelBuilder = spy(new MetamodelBuilder(new Configuration()));

    @Test
    void processingNonTransientFieldWithoutPropertyInfoThrowsException() throws Exception {
        final IdentifiableEntityType<InvalidClass> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(InvalidClass.class);
        when(metamodelBuilder.hasManagedType(InvalidClass.class)).thenReturn(true);
        final ClassFieldMetamodelProcessor<InvalidClass> processor = new ClassFieldMetamodelProcessor<>(new TypeBuilderContext<>(etMock, new NamespaceResolver()), metamodelBuilder);
        final Field field = InvalidClass.class.getDeclaredField("invalidAttribute");
        assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));
    }

    private static final class InvalidClass {

        private String invalidAttribute;    // Attribute not transient but has no property/id info

        public String getInvalidAttribute() {
            return invalidAttribute;
        }

        public void setInvalidAttribute(String invalidAttribute) {
            this.invalidAttribute = invalidAttribute;
        }
    }

    @Test
    void processDynamicObjectFieldIsResolvedAsSingularAttributeImpl() throws Exception {
        final IdentifiableEntityType<OWLClassV> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(OWLClassV.class);
        final ClassFieldMetamodelProcessor<OWLClassV> processor = prepareProcessorForClass(etMock);
        final Field field = OWLClassV.getSingularDynamicAttField();
        when(metamodelBuilder.hasManagedType(OWLClassV.class)).thenReturn(true);

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(etMock).addDeclaredAttribute(eq(field.getName()), captor.capture());

        assertInstanceOf(SingularAttributeImpl.class, captor.getValue());
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "singularDynamicAttribute", captor.getValue().getIRI().toString());
    }

    @Test
    void processDynamicObjectFieldIsResolvedAsPluralAttributeImpl() throws Exception {
        final IdentifiableEntityType<OWLClassV> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(OWLClassV.class);
        final ClassFieldMetamodelProcessor<OWLClassV> processor = prepareProcessorForClass(etMock);
        final Field field = OWLClassV.getPluralDynamicAttField();
        when(metamodelBuilder.hasManagedType(OWLClassV.class)).thenReturn(true);

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(etMock).addDeclaredAttribute(eq(field.getName()), captor.capture());

        assertInstanceOf(SingularAttributeImpl.class, captor.getValue());
        assertEquals(Vocabulary.ATTRIBUTE_BASE + "pluralDynamicAttribute", captor.getValue().getIRI().toString());
    }

    @Test
    void processPluralFieldWithNonEmptyCardinalityConstraintAddsTheConstraintToAttributeSpecification() throws Exception {
        final IdentifiableEntityType<OWLClassJ> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        final ClassFieldMetamodelProcessor<OWLClassJ> processor = prepareProcessorForClass(etMock);
        final Field field = OWLClassJ.getOwlClassAField();
        when(metamodelBuilder.hasManagedType(OWLClassJ.class)).thenReturn(true);
        when(metamodelBuilder.hasManagedType(OWLClassA.class)).thenReturn(true);

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(etMock).addDeclaredAttribute(eq(field.getName()), captor.capture());

        assertNotNull(captor.getValue());
    }

    @Test
    void findPropertyDefinitionInHierarchyFindsAttributeDeeplyNested() throws Exception {
        final IdentifiableEntityType<OWLClassY> baseMock = mock(IdentifiableEntityType.class);

        final AbstractIdentifiableType<OWLInterfaceD> dInterfaceMock = mock(AbstractIdentifiableType.class);
        final AbstractIdentifiableType<OWLInterfaceE> eInterfaceMock = mock(AbstractIdentifiableType.class);
        final AbstractIdentifiableType<OWLInterfaceC> cInterfaceMock = mock(AbstractIdentifiableType.class);
        final Set<AbstractIdentifiableType<? super OWLClassY>> classSuperTypes = new HashSet<>();

        classSuperTypes.add(cInterfaceMock);
        classSuperTypes.add(eInterfaceMock);


        when(baseMock.getJavaType()).thenReturn(OWLClassY.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);
        when(eInterfaceMock.getSupertypes()).thenReturn(Collections.singleton(dInterfaceMock));
        final ClassFieldMetamodelProcessor<OWLClassY> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassY.getNameField();
        when(metamodelBuilder.getAnnotatedAccessorsForClass(dInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceD.getSetNameMethod())));

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(baseMock).addDeclaredAttribute(eq(field.getName()), captor.capture());
        assertEquals(captor.getValue().getIRI().toString(), Vocabulary.ATTRIBUTE_BASE + "name");
    }


    @Test
    void findPropertyDefinitionInHierarchyThrowsExceptionIfAmbiguous() throws Exception {
        final IdentifiableEntityType<OWLClassX> baseMock = mock(IdentifiableEntityType.class);

        final AbstractIdentifiableType<OWLInterfaceB> BInterfaceMock = mock(AbstractIdentifiableType.class);
        final AbstractIdentifiableType<OWLInterfaceA> AInterfaceMock = mock(AbstractIdentifiableType.class);

        final Set<AbstractIdentifiableType<? super OWLClassX>> classSuperTypes = new HashSet<>();
        classSuperTypes.add(BInterfaceMock);
        classSuperTypes.add(AInterfaceMock);

        final ClassFieldMetamodelProcessor<OWLClassX> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassX.getPropertyField();

        when(baseMock.getJavaType()).thenReturn(OWLClassX.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);
        when(metamodelBuilder.getAnnotatedAccessorsForClass(BInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceB.getPropertyMethod())));
        when(metamodelBuilder.getAnnotatedAccessorsForClass(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.getPropertyMethod())));

        MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));
        assertTrue(ex.getMessage().contains("Ambiguous hierarchy"));

    }

    @Test
    void findPropertyDefinitionInHierarchyThrowsExceptionIfMultipleAnnotationsDoNotEqual() throws Exception {
        final IdentifiableEntityType<OWLClassZ> baseMock = mock(IdentifiableEntityType.class);
        final AbstractIdentifiableType<OWLInterfaceA> AInterfaceMock = mock(AbstractIdentifiableType.class);


        Set<AnnotatedAccessor> owlClassZAccessors = new HashSet<>();
        owlClassZAccessors.add(AnnotatedAccessor.from(OWLClassZ.getAmbiguousPropertyGetMethod()));
        owlClassZAccessors.add(AnnotatedAccessor.from(OWLClassZ.getAmbiguousPropertySetMethod()));

        final ClassFieldMetamodelProcessor<OWLClassZ> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassZ.getAmbiguousField();

        when(baseMock.getJavaType()).thenReturn(OWLClassZ.class);
        when(baseMock.getSupertypes()).thenReturn(Collections.singleton(AInterfaceMock));

        when(metamodelBuilder.getAnnotatedAccessorsForClass(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.getPropertyMethod())));
        when(metamodelBuilder.getAnnotatedAccessorsForClass(baseMock)).thenReturn(owlClassZAccessors);

        MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));
        assertTrue(ex.getMessage().contains("Ambiguous hierarchy"));

    }

    @Test
    void findPropertyDefinitionInHierarchyDoesNotThrowExceptionIfAnnotationsEqual() throws Exception {
        final IdentifiableEntityType<OWLClasAA> baseMock = mock(IdentifiableEntityType.class);

        final AbstractIdentifiableType<OWLInterfaceF> FInterfaceMock = mock(AbstractIdentifiableType.class);
        final AbstractIdentifiableType<OWLInterfaceG> GInterfaceMock = mock(AbstractIdentifiableType.class);

        final Set<AbstractIdentifiableType<? super OWLClasAA>> classSuperTypes = new HashSet<>();


        classSuperTypes.add(FInterfaceMock);
        classSuperTypes.add(GInterfaceMock);

        final ClassFieldMetamodelProcessor<OWLClasAA> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClasAA.getPropertyField();

        when(baseMock.getJavaType()).thenReturn(OWLClasAA.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);
        when(metamodelBuilder.getAnnotatedAccessorsForClass(FInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceF.getPropertyMethod())));
        when(metamodelBuilder.getAnnotatedAccessorsForClass(GInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceG.getPropertyMethod())));

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(baseMock).addDeclaredAttribute(eq(field.getName()), captor.capture());

        assertNotNull(captor.getValue());
    }

    @Test
    void findPropertyDefinitionInHierarchyDoesNotMatchMismatchingTypes() throws Exception {
        final IdentifiableEntityType<OWLClassZ> baseMock = mock(IdentifiableEntityType.class);

        final AbstractIdentifiableType<OWLInterfaceA> AInterfaceMock = mock(AbstractIdentifiableType.class);

        when(baseMock.getJavaType()).thenReturn(OWLClassZ.class);
        when(baseMock.getSupertypes()).thenReturn(Collections.singleton(AInterfaceMock));

        final ClassFieldMetamodelProcessor<OWLClassZ> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassZ.getPropertyField();
        when(metamodelBuilder.getAnnotatedAccessorsForClass(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.getPropertyMethod())));

        assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));

    }

    @Test
    void findPropertyDefinitionInHierarchyFindsDefinitionInSameClass() throws NoSuchFieldException, NoSuchMethodException {
        final IdentifiableEntityType<OWLClassWithAnnotatedPropertyAndMethod> baseMock = mock(IdentifiableEntityType.class);

        final AbstractIdentifiableType<OWLInterfaceA> AInterfaceMock = mock(AbstractIdentifiableType.class);

        final Set<AbstractIdentifiableType<? super OWLClassWithAnnotatedPropertyAndMethod>> classSuperTypes = new HashSet<>();

        classSuperTypes.add(AInterfaceMock);

        when(baseMock.getJavaType()).thenReturn(OWLClassWithAnnotatedPropertyAndMethod.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);

        final ClassFieldMetamodelProcessor<OWLClassWithAnnotatedPropertyAndMethod> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassWithAnnotatedPropertyAndMethod.getIsNiceField();

        when(metamodelBuilder.getAnnotatedAccessorsForClass(baseMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLClassWithAnnotatedPropertyAndMethod.getIsNiceMethod())));
        when(metamodelBuilder.getAnnotatedAccessorsForClass(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.getPropertyMethod())));

        processor.processField(field);

        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(baseMock).addDeclaredAttribute(eq(field.getName()), captor.capture());

        assertNotNull(captor.getValue());
    }

    @Test
    void fieldProcessingFailsOnNonCompatibleTypes() throws NoSuchFieldException, NoSuchMethodException {
        final IdentifiableEntityType<OWLClassWithWrongTypes> baseMock = mock(IdentifiableEntityType.class);

        when(baseMock.getJavaType()).thenReturn(OWLClassWithWrongTypes.class);
        when(baseMock.getSupertypes()).thenReturn(Collections.emptySet());

        final ClassFieldMetamodelProcessor<OWLClassWithWrongTypes> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassWithWrongTypes.getCountField();

        when(metamodelBuilder.getAnnotatedAccessorsForClass(baseMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLClassWithWrongTypes.getSetCountMethod())));

        MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));
        assertTrue(ex.getMessage().contains("Non-compatible types"));
    }

    @Test
    void processFieldHandlesPrefixUseInSequenceAnnotation() throws Exception {
        final IdentifiableEntityType<OWLClassWithRdfList> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(OWLClassWithRdfList.class);
        final ClassFieldMetamodelProcessor<OWLClassWithRdfList> sut = prepareProcessorForClass(etMock);

        sut.processField(OWLClassWithRdfList.class.getDeclaredField("rdfList"));
        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(etMock).addDeclaredAttribute(eq("rdfList"), captor.capture());
        assertInstanceOf(ListAttributeImpl.class, captor.getValue());
        final ListAttributeImpl<?, ?> result = (ListAttributeImpl<?, ?>) captor.getValue();
        assertEquals(SequenceType.referenced, result.getSequenceType());
        assertEquals(RDF.LIST, result.getListClassIRI().toString());
        assertEquals(RDF.FIRST, result.getHasContentsPropertyIRI().toString());
        assertEquals(RDF.REST, result.getHasNextPropertyIRI().toString());
    }

    // Test classes

    private <X> ClassFieldMetamodelProcessor<X> prepareProcessorForClass(IdentifiableEntityType<X> etMock) {
        final TypeBuilderContext<X> context = new TypeBuilderContext<>(etMock, new NamespaceResolver());
        context.setConverterResolver(new ConverterResolver(new Converters(new Configuration())));
        return new ClassFieldMetamodelProcessor<>(context, metamodelBuilder);
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassX")
    private static class OWLClassX implements OWLInterfaceA, OWLInterfaceB {
        @Id
        private URI uri;

        private String property;

        @Override
        public String getProperty() {
            return property;
        }

        public static Field getPropertyField() throws NoSuchFieldException {
            return OWLClassX.class.getDeclaredField("property");
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceF")
    public interface OWLInterfaceF {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyA", lexicalForm = true)
        String getProperty();

        static Method getPropertyMethod() throws NoSuchMethodException {
            return OWLInterfaceF.class.getMethod("getProperty");
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceG")
    private interface OWLInterfaceG {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyA", lexicalForm = true)
        String getProperty();

        static Method getPropertyMethod() throws NoSuchMethodException {
            return OWLInterfaceG.class.getMethod("getProperty");
        }
    }


    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassAA")
    private static class OWLClasAA implements OWLInterfaceF, OWLInterfaceG {
        @Id
        private URI uri;

        private String property;

        @Override
        public String getProperty() {
            return property;
        }

        public static Field getPropertyField() throws NoSuchFieldException {
            return OWLClasAA.class.getDeclaredField("property");
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceA")
    public interface OWLInterfaceA {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyA")
        String getProperty();

        static Method getPropertyMethod() throws NoSuchMethodException {
            return OWLInterfaceA.class.getMethod("getProperty");
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceB")
    private interface OWLInterfaceB {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyB")
        String getProperty();

        static Method getPropertyMethod() throws NoSuchMethodException {
            return OWLInterfaceB.class.getMethod("getProperty");
        }
    }


    @OWLClass(iri = Vocabulary.CLASS_BASE + "ChildClassWithMultipleParents")
    private static class OWLClassY implements OWLInterfaceE, OWLInterfaceC {
        @Id
        private URI uri;

        private String name;

        @Override
        public void setName(String name) {
        }

        static Field getNameField() throws NoSuchFieldException {
            return OWLClassY.class.getDeclaredField("name");
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceE")
    private interface OWLInterfaceE extends OWLInterfaceD {

    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceC")
    private interface OWLInterfaceC {
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceD")
    private interface OWLInterfaceD {

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "name")
        void setName(String name);

        static Method getSetNameMethod() throws NoSuchMethodException {
            return OWLInterfaceD.class.getMethod("setName", String.class);
        }

    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassZ")
    private static class OWLClassZ implements OWLInterfaceA {
        private Boolean property;

        private String ambiguousProperty;

        public static Field getPropertyField() throws NoSuchFieldException {
            return OWLClassZ.class.getDeclaredField("property");
        }

        public static Field getAmbiguousField() throws NoSuchFieldException {
            return OWLClassZ.class.getDeclaredField("ambiguousProperty");
        }

        @OWLDataProperty(iri = Vocabulary.CLASS_BASE + "ambiguousProperty")
        @ParticipationConstraints(nonEmpty = false)
        public String getAmbiguousProperty() {
            return ambiguousProperty;
        }

        @OWLDataProperty(iri = Vocabulary.CLASS_BASE + "ambiguousProperty")
        @ParticipationConstraints(nonEmpty = true)
        public void setAmbiguousProperty(String ambiguousProperty) {
            this.ambiguousProperty = ambiguousProperty;
        }

        static Method getAmbiguousPropertyGetMethod() throws NoSuchMethodException {
            return OWLClassZ.class.getMethod("getAmbiguousProperty");
        }

        static Method getAmbiguousPropertySetMethod() throws NoSuchMethodException {
            return OWLClassZ.class.getMethod("setAmbiguousProperty", String.class);
        }

        @Override
        public String getProperty() {
            return null;
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassWithAnnotatedPropertyAndMethod")
    private static class OWLClassWithAnnotatedPropertyAndMethod implements OWLInterfaceA {
        private String property;

        private Boolean isNice;

        public static Field getIsNiceField() throws NoSuchFieldException {
            return OWLClassWithAnnotatedPropertyAndMethod.class.getDeclaredField("isNice");
        }

        public static Method getIsNiceMethod() throws NoSuchMethodException {
            return OWLClassWithAnnotatedPropertyAndMethod.class.getMethod("getIsNice");
        }

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "isNice")
        public Boolean getIsNice() {
            return isNice;
        }

        public String getProperty() {
            return property;
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassWithWrongTypes")
    private static class OWLClassWithWrongTypes{
        private Integer count;

        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "count")
        public void setCount(String  count){
            System.out.println("NOP");
        }
        public static Field getCountField() throws NoSuchFieldException {
            return OWLClassWithWrongTypes.class.getDeclaredField("count");
        }

        public static Method getSetCountMethod() throws NoSuchMethodException {
            return OWLClassWithWrongTypes.class.getMethod("setCount", String.class);
        }
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassWithRdfList")
    private static class OWLClassWithRdfList {
        @Sequence(type = SequenceType.referenced, listClassIRI = "rdf:List", hasContentsPropertyIRI = "rdf:first", hasNextPropertyIRI = "rdf:rest")
        @OWLDataProperty(iri = Vocabulary.ATTRIBUTE_BASE + "rdflist")
        private List<String> rdfList;
    }
}


