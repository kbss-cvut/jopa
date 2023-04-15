/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.Vocabulary;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLAnnotationProperty;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ClassFieldMetamodelProcessorTest {

    private final MetamodelBuilder metamodelBuilder = spy(new MetamodelBuilder(new Configuration()));

    @Test
    void processingNonTransientFieldWithoutPropertyInfoThrowsException() throws Exception {
        final IdentifiableEntityType<InvalidClass> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(InvalidClass.class);
        when(metamodelBuilder.hasManagedType(InvalidClass.class)).thenReturn(true);
        final ClassFieldMetamodelProcessor<InvalidClass> processor =
                new ClassFieldMetamodelProcessor<>(new TypeBuilderContext<>(etMock, new NamespaceResolver()),
                        metamodelBuilder);
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
    void processPluralFieldWithNonEmptyCardinalityConstraintAddsTheConstraintToAttributeSpecification()
            throws Exception {
        final IdentifiableEntityType<OWLClassJ> etMock = mock(IdentifiableEntityType.class);
        when(etMock.getJavaType()).thenReturn(OWLClassJ.class);
        final ClassFieldMetamodelProcessor<OWLClassJ> processor = prepareProcessorForClass(etMock);
        final Field field = OWLClassJ.getOwlClassAField();
        when(metamodelBuilder.hasManagedType(OWLClassJ.class)).thenReturn(true);
        when(metamodelBuilder.hasManagedType(OWLClassA.class)).thenReturn(true);
        processor.processField(field);
        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(etMock).addDeclaredAttribute(eq(field.getName()), captor.capture());
        assertTrue(captor.getValue().isNonEmpty());
    }

    @Test
    void findPropertyDefinitionInHierarchyFindsAttributeDeeplyNested()
            throws Exception {
        final IdentifiableEntityType<OWLClassY> baseMock = mock(IdentifiableEntityType.class);

        final IdentifiableType<OWLInterfaceD> dInterfaceMock = mock(IdentifiableType.class);
        final IdentifiableType<OWLInterfaceE> eInterfaceMock = mock(IdentifiableType.class);
        final IdentifiableType<OWLInterfaceC> cInterfaceMock = mock(IdentifiableType.class);
        final Set<IdentifiableType<? super OWLClassY>> classSuperTypes = new HashSet<>();


        classSuperTypes.add(cInterfaceMock);
        classSuperTypes.add(eInterfaceMock);


        when(baseMock.getJavaType()).thenReturn(OWLClassY.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);
        when(eInterfaceMock.getSupertypes()).thenReturn(Collections.singleton(dInterfaceMock));
        final ClassFieldMetamodelProcessor<OWLClassY> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassY.getNameField();
        when(metamodelBuilder.getTypesPropertyMethods(dInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceD.class.getMethod("setName", String.class))));

        processor.processField(field);


        final ArgumentCaptor<AbstractAttribute> captor = ArgumentCaptor.forClass(AbstractAttribute.class);
        verify(baseMock).addDeclaredAttribute(eq(field.getName()), captor.capture());
        assertEquals(captor.getValue().getIRI().toString(), Vocabulary.ATTRIBUTE_BASE + "name");
    }


    @Test
    void findPropertyDefinitionInHierarchyThrowsExceptionIfAmbiguous()
            throws Exception {
        final IdentifiableEntityType<OWLClassX> baseMock = mock(IdentifiableEntityType.class);

        final IdentifiableType<OWLInterfaceB> BInterfaceMock = mock(IdentifiableType.class);
        final IdentifiableType<OWLInterfaceA> AInterfaceMock = mock(IdentifiableType.class);

        final Set<IdentifiableType<? super OWLClassX>> classSuperTypes = new HashSet<>();


        classSuperTypes.add(BInterfaceMock);
        classSuperTypes.add(AInterfaceMock);


        when(baseMock.getJavaType()).thenReturn(OWLClassX.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);

        final ClassFieldMetamodelProcessor<OWLClassX> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassX.getPropertyField();
        when(metamodelBuilder.getTypesPropertyMethods(BInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceB.class.getMethod("getProperty"))));
        when(metamodelBuilder.getTypesPropertyMethods(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.class.getMethod("getProperty"))));

        MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));
        assertTrue(ex.getMessage().contains("Ambiguous hierarchy"));

    }

    @Test
    void findPropertyDefinitionInHierarchyDoesNotThrowExceptionIfAnnotationsEqual()
            throws Exception {
        final IdentifiableEntityType<OWLClasAA> baseMock = mock(IdentifiableEntityType.class);

        final IdentifiableType<OWLInterfaceF> FInterfaceMock = mock(IdentifiableType.class);
        final IdentifiableType<OWLInterfaceG> GInterfaceMock = mock(IdentifiableType.class);

        final Set<IdentifiableType<? super OWLClasAA>> classSuperTypes = new HashSet<>();


        classSuperTypes.add(FInterfaceMock);
        classSuperTypes.add(GInterfaceMock);


        when(baseMock.getJavaType()).thenReturn(OWLClasAA.class);
        when(baseMock.getSupertypes()).thenReturn(classSuperTypes);

        final ClassFieldMetamodelProcessor<OWLClasAA> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClasAA.getPropertyField();
        when(metamodelBuilder.getTypesPropertyMethods(FInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceF.class.getMethod("getProperty"))));
        when(metamodelBuilder.getTypesPropertyMethods(GInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceG.class.getMethod("getProperty"))));

        processor.processField(field);


    }

    @Test
    void findPropertyDefinitionInHierarchyDoesNotMatchMismatchingTypes()
            throws Exception {
        final IdentifiableEntityType<OWLClassZ> baseMock = mock(IdentifiableEntityType.class);

        final IdentifiableType<OWLInterfaceA> AInterfaceMock = mock(IdentifiableType.class);


        when(baseMock.getJavaType()).thenReturn(OWLClassZ.class);
        when(baseMock.getSupertypes()).thenReturn(Collections.singleton(AInterfaceMock));

        final ClassFieldMetamodelProcessor<OWLClassZ> processor = prepareProcessorForClass(baseMock);
        final Field field = OWLClassZ.getPropertyField();
        when(metamodelBuilder.getTypesPropertyMethods(AInterfaceMock)).thenReturn(Collections.singleton(AnnotatedAccessor.from(OWLInterfaceA.class.getMethod("getProperty"))));

        assertThrows(MetamodelInitializationException.class, () -> processor.processField(field));

    }

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
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceG")
    private interface OWLInterfaceG {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyA", lexicalForm = true)
        String getProperty();
    }


    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLClassX")
    private static class OWLClasAA implements OWLInterfaceF, OWLInterfaceG {
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

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceA")
    public interface OWLInterfaceA {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyA")
        String getProperty();
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "OWLInterfaceB")
    private interface OWLInterfaceB {
        @OWLAnnotationProperty(iri = Vocabulary.ATTRIBUTE_BASE + "propertyB")
        String getProperty();
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
    }

    private static class OWLClassZ implements OWLInterfaceA {
        private Boolean property;

        public static Field getPropertyField() throws NoSuchFieldException {
            return OWLClassZ.class.getDeclaredField("property");
        }

        @Override
        public String getProperty() {
            return null;
        }
    }
}


