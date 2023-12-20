/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AbstractIdentifiableTypeTest {

    private static final String ID_NAME = "uri";

    private static Class<OWLClassA> cls;
    private static IRI classIri;
    private static String className;

    private IdentifiableEntityType<OWLClassA> et;

    @BeforeAll
    static void setUpBeforeClass() {
        cls = OWLClassA.class;
        classIri = IRI.create(OWLClassA.getClassIri());
        className = OWLClassA.class.getName();
    }

    @BeforeEach
    void setUp() {
        this.et = new ConcreteEntityType<>(className, cls, classIri);
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
        verify(supertype).getAttributes();
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
        doReturn(att).when(supertype).getAttribute(attName);
        assertEquals(att, et.getAttribute(attName));
        verify(supertype).getAttribute(attName);
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
        when(supertype.getAttributes()).thenReturn(Collections.singleton(supertypeAtt));
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
    void getFieldSpecificationGetsFieldSpecification() {
        final SingularAttribute supertypeAtt = mock(SingularAttribute.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertypes(Collections.singleton(supertype));
        final String attName = "test";
        doReturn(supertypeAtt).when(supertype).getFieldSpecification(attName);
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
        assertTrue(idSpec instanceof Identifier);
    }
}
