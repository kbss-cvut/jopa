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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.hamcrest.Matcher;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.hasItems;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class AbstractIdentifiableTypeTest {

    private static final String ID_NAME = "uri";

    private static Class<OWLClassA> cls;
    private static IRI classIri;

    private IdentifiableEntityType<OWLClassA> et;

    @BeforeAll
    static void setUpBeforeClass() {
        cls = OWLClassA.class;
        classIri = IRI.create(OWLClassA.getClassIri());
    }

    @BeforeEach
    void setUp() {
        this.et = new ConcreteEntityType<>(cls, cls, classIri);
        final Identifier<OWLClassA, URI> id = mock(Identifier.class);
        when(id.getName()).thenReturn(ID_NAME);
        et.setIdentifier(id);
    }

    @Test
    void hasSingleIdAttributeReturnsAlwaysTrue() {
        assertTrue(et.hasSingleIdAttribute());
    }

    @Test
    void getAttributesReturnsDeclaredAttributesPlusInheritedAttributes() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        et.getAttributes();
        verify(supertype).getAttributes(cls);
    }

    @Test
    void getAttributesReturnsDeclaredAttributesWhenThereIsNoSupertype() {
        final AbstractAttribute<OWLClassA, ?> att = mock(AbstractAttribute.class);
        et.addDeclaredAttribute("test", att);
        final Set<Attribute<? super OWLClassA, ?>> result = et.getAttributes();
        assertEquals(1, result.size());
        assertTrue(result.contains(att));
    }

    @Test
    void getAttributeReturnsInheritedAttribute() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        final String attName = "test";
        final AbstractAttribute att = mock(AbstractAttribute.class);
        doReturn(att).when(supertype).getAttribute(attName, cls);
        assertEquals(att, et.getAttribute(attName));
        verify(supertype).getAttribute(attName, cls);
    }

    @Test
    void getAttributeThrowsIllegalArgumentWhenAttributeIsNotFoundInDeclaredAndInheritedAttributes() {
        final String attName = "unknownAttribute";
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getAttribute(attName));
        assertEquals("Attribute " + attName + " is not present in type " + et.toString(), ex.getMessage());
    }

    @Test
    void getCollectionThrowsIllegalArgumentWhenElementTypeDoesNotMatch() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getCollection(attName, OWLClassB.class));
        assertEquals("Collection attribute " + attName + " with element type " + OWLClassB.class +
                " is not present in type " + et.toString(), ex.getMessage());
    }

    @Test
    void getListReturnsListAttribute() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final ListAttribute result = et.getList(attName, OWLClassA.class);
        assertEquals(att, result);
    }

    @Test
    void getSetThrowsIllegalArgumentWhenAttributeIsNotSet() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getSet(attName, OWLClassA.class));
        assertEquals("Set attribute " + attName + " with element type " + OWLClassA.class +
                " is not present in type " + et.toString(), ex.getMessage());
    }

    @Test
    void getPluralAttributesGetsAlsoInheritedPluralAttributes() {
        final CollectionAttribute att = mock(CollectionAttribute.class);
        when(att.isCollection()).thenReturn(true);
        final ListAttributeImpl listAtt = mock(ListAttributeImpl.class);
        when(listAtt.isCollection()).thenReturn(true);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        et.addDeclaredAttribute("test", listAtt);
        when(supertype.getPluralAttributes()).thenReturn(Collections.singleton(att));
        final Set<PluralAttribute<? super OWLClassA, ?, ?>> result = et.getPluralAttributes();
        assertTrue(result.contains(att));
        assertTrue(result.contains(listAtt));
    }

    @Test
    void getSingularAttributesGetsAlsoInheritedSingularAttributes() {
        final SingularAttributeImpl attOne = mock(SingularAttributeImpl.class);
        when(attOne.isCollection()).thenReturn(false);
        final SingularAttributeImpl attTwo = mock(SingularAttributeImpl.class);
        when(attTwo.isCollection()).thenReturn(false);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        et.addDeclaredAttribute("test", attTwo);
        when(supertype.getSingularAttributes()).thenReturn(Collections.singleton(attOne));
        final Set<SingularAttribute<? super OWLClassA, ?>> result = et.getSingularAttributes();
        assertTrue(result.contains(attOne));
        assertTrue(result.contains(attTwo));
    }

    @Test
    void getSingularAttributeReturnsAttributeWithMatchingNameAndType() {
        final SingularAttributeImpl attOne = mock(SingularAttributeImpl.class);
        when(attOne.isCollection()).thenReturn(false);
        final String attName = "test";
        when(attOne.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, attOne);
        final SingularAttribute result = et.getSingularAttribute(attName, OWLClassA.class);
        assertEquals(attOne, result);
    }

    @Test
    void getSingularAttributeThrowsIllegalArgumentWhenAttributeIsNotSingular() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getSingularAttribute(attName, OWLClassB.class));
        assertEquals("Singular attribute " + attName + " of type " + OWLClassB.class + " is not present in type " +
                et.toString(), ex.getMessage());
    }

    @Test
    void getDeclaredAttributeReturnsAttributeDeclaredInType() {
        final AbstractAttribute attOne = mock(AbstractAttribute.class);
        when(attOne.isCollection()).thenReturn(false);
        final String attName = "test";
        when(attOne.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, attOne);
        final Attribute result = et.getDeclaredAttribute(attName);
        assertEquals(attOne, result);
    }

    @Test
    void getDeclaredCollectionReturnsDeclaredCollectionAttribute() {
        final AbstractPluralAttribute att = mock(CollectionAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final CollectionAttribute result = et.getDeclaredCollection(attName);
        assertEquals(att, result);
    }

    @Test
    void getDeclaredListThrowsIllegalArgumentWhenListElementTypeDoesNotMatch() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getDeclaredList(attName, OWLClassB.class));
        assertEquals(
                "List attribute " + attName + " with element type " + OWLClassB.class + " is not declared in type " +
                        et.toString(), ex.getMessage());
    }

    @Test
    void getDeclaredSetThrowsIllegalArgumentWhenAttributeIsNotSet() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getDeclaredSet(attName, OWLClassB.class));
        assertEquals(
                "Set attribute " + attName + " with element type " + OWLClassB.class + " is not declared in type " +
                        et.toString(), ex.getMessage());
    }

    @Test
    void getDeclaredSingularAttributeReturnsDeclaredSingularAttribute() {
        final SingularAttributeImpl att = mock(SingularAttributeImpl.class);
        final String attName = "test";
        when(att.isCollection()).thenReturn(false);
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final SingularAttribute result = et.getDeclaredSingularAttribute(attName);
        assertEquals(att, result);
    }

    @Test
    void getDeclaredSingularAttributeThrowsIllegalArgumentForInvalidType() {
        final AbstractAttribute att = mock(AbstractAttribute.class);
        final String attName = "test";
        when(att.isCollection()).thenReturn(false);
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getDeclaredSingularAttribute(attName, OWLClassB.class));
        assertEquals("Singular attribute " + attName + " of type " + OWLClassB.class + " is not declared in type " +
                et.toString(), ex.getMessage());
    }

    @Test
    void getFieldSpecificationsReturnsAllAttributesAndTypesAndProperties() {
        final SingularAttribute supertypeAtt = mock(SingularAttribute.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        when(supertype.getAttributes(any())).thenReturn(Collections.singleton(supertypeAtt));
        final ListAttributeImpl listAtt = mock(ListAttributeImpl.class);
        et.addDeclaredAttribute("list", listAtt);
        final TypesSpecification types = mock(TypesSpecification.class);
        et.addDirectTypes(types);
        final PropertiesSpecification properties = mock(PropertiesSpecification.class);
        et.addOtherProperties(properties);

        final Set<FieldSpecification<? super OWLClassA, ?>> result = et.getFieldSpecifications();
        assertTrue(result.contains(supertypeAtt));
        assertTrue(result.contains(listAtt));
        assertTrue(result.contains(types));
        assertTrue(result.contains(properties));
    }

    @Test
    void getFieldSpecificationsReturnsAlsoIdentifier() {
        final Set<FieldSpecification<? super OWLClassA, ?>> result = et.getFieldSpecifications();
        final Optional<FieldSpecification<? super OWLClassA, ?>> id = result.stream()
                                                                            .filter(fs -> fs.equals(et.getIdentifier()))
                                                                            .findAny();
        assertTrue(id.isPresent());
    }

    @Test
    void getTypesReturnsTypesAlsoFromSuperType() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        final TypesSpecification types = mock(TypesSpecification.class);
        when(supertype.getTypes()).thenReturn(types);
        final TypesSpecification result = et.getTypes();
        assertEquals(types, result);
    }

    @Test
    void getPropertiesReturnsPropertiesAlsoFromSuperType() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        final PropertiesSpecification properties = mock(PropertiesSpecification.class);
        when(supertype.getProperties()).thenReturn(properties);
        final PropertiesSpecification result = et.getProperties();
        assertEquals(properties, result);
    }

    @Test
    void getFieldSpecificationGetsFieldSpecificationFromSupertype() {
        final SingularAttribute supertypeAtt = mock(SingularAttribute.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        final String attName = "test";
        doReturn(supertypeAtt).when(supertype).getFieldSpecification(attName, cls);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(supertypeAtt, result);
    }

    @Test
    void getFieldSpecificationGetsTypes() {
        final TypesSpecification types = mock(TypesSpecification.class);
        final String attName = "types";
        when(types.getName()).thenReturn(attName);
        et.addDirectTypes(types);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(types, result);
    }

    @Test
    void getFieldSpecificationGetsDeclaredAttribute() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        et.addDeclaredAttribute(attName, att);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(att, result);
    }

    @Test
    void getFieldSpecificationThrowsIllegalArgumentWhenFieldIsNotFound() {
        final String attName = "unknownAttribute";
        final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> et.getFieldSpecification(attName));
        assertEquals("Field " + attName + " is not present in type " + et.toString(), ex.getMessage());
    }

    @Test
    void getIdentifierReturnsIdentifierFromSuperclass() {
        final Identifier id = mock(Identifier.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        doReturn(id).when(supertype).getIdentifier();
        et.setSupertypes(Collections.singleton(supertype));
        et.setIdentifier(null);
        assertEquals(id, et.getIdentifier());
    }

    @Test
    void getFieldSpecificationReturnsIdentifier() {
        final FieldSpecification<? super OWLClassA, ?> idSpec = et.getFieldSpecification("uri");
        assertInstanceOf(Identifier.class, idSpec);
    }

    @Test
    void getAttributeResolvesGenericAttributeFromSupertype() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Attribute<?, ?> result = child.getAttribute("boss");
        assertEquals(att, result);
    }

    @Test
    void getAttributesIncludesGenericAttributes() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Set<Attribute<? super MetamodelBuilderTest.ConcreteClassWithGenericType, ?>> result = child.getAttributes();
        assertThat(result, hasItem(att));
    }

    @Test
    void getAttributeOfGenericSupertypeReturnsFirstAvailableTypedGenericAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Attribute<?, ?> result = parent.getAttribute("boss");
        assertThat(Set.of(att, attII), hasItem(result));
        final Attribute<MetamodelBuilderTest.ClassWithGenericType, ?> declaredResult = parent.getDeclaredAttribute("boss");
        assertThat(Set.of(att, attII), hasItem(declaredResult));
    }

    @Test
    void getAttributesOfGenericSupertypeIncludesFirstAvailableTypedGenericAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Set<Attribute<? super MetamodelBuilderTest.ClassWithGenericType, ?>> result = parent.getAttributes();
        assertEquals(1, result.size());
        assertThat(Set.of(att, attII), hasItem(result.iterator().next()));
        final Set<Attribute<MetamodelBuilderTest.ClassWithGenericType, ?>> declaredResult = parent.getDeclaredAttributes();
        assertEquals(1, declaredResult.size());
        assertThat(Set.of(att, attII), hasItem(declaredResult.iterator().next()));
    }

    @Test
    void getFieldSpecificationResolvesGenericAttributeFromSupertype() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final FieldSpecification<?, ?> result = child.getFieldSpecification("boss");
        assertEquals(att, result);
    }

    @Test
    void getFieldSpecificationInSupertypeReturnsFirstAvailableTypedGenericAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final FieldSpecification<?, ?> result = parent.getFieldSpecification("boss");
        assertThat(Set.of(att, attII), hasItem(result));
    }

    @Test
    void getFieldSpecificationsIncludesGenericAttributes() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        final Identifier<MetamodelBuilderTest.ClassWithGenericType, URI> identifier = mock(Identifier.class);
        parent.setIdentifier(identifier);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Set<FieldSpecification<? super MetamodelBuilderTest.ConcreteClassWithGenericType, ?>> result = child.getFieldSpecifications();
        assertThat(result, hasItem(att));
    }

    @Test
    void getFieldSpecificationsInSupertypeIncludesFirstAvailableTypedGenericAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericType> parent = new AbstractEntityType<>(MetamodelBuilderTest.ClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                               .iri()));
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithGenericType> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithGenericType.class, MetamodelBuilderTest.ConcreteClassWithGenericType.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithGenericType.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                               .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassA> att = mock(AbstractAttribute.class);
        final AbstractAttribute<MetamodelBuilderTest.ClassWithGenericType, OWLClassB> attII = mock(AbstractAttribute.class);
        final Identifier<MetamodelBuilderTest.ClassWithGenericType, URI> identifier = mock(Identifier.class);
        parent.setIdentifier(identifier);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericType.class, att);
        parent.addDeclaredGenericAttribute("boss", MetamodelBuilderTest.ConcreteClassWithGenericTypeII.class, attII);

        final Set<FieldSpecification<? super MetamodelBuilderTest.ClassWithGenericType, ?>> result = parent.getFieldSpecifications();
        assertThat(result, anyOf((Matcher<? super Set<FieldSpecification<? super MetamodelBuilderTest.ClassWithGenericType, ?>>>) hasItem(att), hasItem(attII)));
    }

    @Test
    void getQueryAttributeResolvesGenericAttributeFromSupertype() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute> parent = new MappedSuperclassTypeImpl<>(MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute.class);
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithQueryAttribute> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                                           .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassA> att = mock(AbstractQueryAttribute.class);
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassB> attII = mock(AbstractQueryAttribute.class);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, att);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttributeII.class, attII);

        final QueryAttribute<?, ?> result = child.getQueryAttribute("related");
        assertEquals(att, result);
        assertTrue(child.hasQueryAttribute("related"));
    }

    @Test
    void getQueryAttributeInSupertypeReturnsFirstAvailableTypedGenericQueryAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute> parent = new MappedSuperclassTypeImpl<>(MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute.class);
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithQueryAttribute> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                                           .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassA> att = mock(AbstractQueryAttribute.class);
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassB> attII = mock(AbstractQueryAttribute.class);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, att);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttributeII.class, attII);

        final QueryAttribute<?, ?> result = parent.getQueryAttribute("related");
        assertThat(Set.of(att, attII), hasItem(result));
        assertTrue(parent.hasQueryAttribute("related"));
    }

    @Test
    void getQueryAttributesIncludesGenericQueryAttributes() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute> parent = new MappedSuperclassTypeImpl<>(MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute.class);
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithQueryAttribute> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                                           .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassA> att = mock(AbstractQueryAttribute.class);
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassB> attII = mock(AbstractQueryAttribute.class);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, att);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttributeII.class, attII);

        final Set<QueryAttribute<? super MetamodelBuilderTest.ConcreteClassWithQueryAttribute, ?>> result = child.getQueryAttributes();
        assertThat(result, hasItem(att));
    }

    @Test
    void getQueryAttributesIncludesFirstAvailableTypedGenericQueryAttribute() {
        final AbstractIdentifiableType<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute> parent = new MappedSuperclassTypeImpl<>(MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute.class);
        final AbstractIdentifiableType<MetamodelBuilderTest.ConcreteClassWithQueryAttribute> child = new ConcreteEntityType<>(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, IRI.create(MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class.getAnnotation(OWLClass.class)
                                                                                                                                                                                                                                                                                                                           .iri()));
        child.setSupertypes(Set.of(parent));
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassA> att = mock(AbstractQueryAttribute.class);
        final AbstractQueryAttribute<MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, OWLClassB> attII = mock(AbstractQueryAttribute.class);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttribute.class, att);
        parent.addDeclaredGenericQueryAttribute("related", MetamodelBuilderTest.ConcreteClassWithQueryAttributeII.class, attII);

        final Set<QueryAttribute<? super MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, ?>> result = parent.getQueryAttributes();
        assertThat(result, anyOf((Matcher<? super Set<QueryAttribute<? super MetamodelBuilderTest.ClassWithGenericTypeAndQueryAttribute, ?>>>) hasItem(att), hasItem(attII)));
    }
}
