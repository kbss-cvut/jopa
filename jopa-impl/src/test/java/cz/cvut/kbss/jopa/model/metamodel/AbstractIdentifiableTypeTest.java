/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.model.IRI;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.net.URI;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.*;

public class AbstractIdentifiableTypeTest {

    private static final String ID_NAME = "uri";

    private static Class<OWLClassA> cls;
    private static IRI classIri;
    private static String className;

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private EntityTypeImpl<OWLClassA> et;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        cls = OWLClassA.class;
        classIri = IRI.create(OWLClassA.getClassIri());
        className = OWLClassA.class.getName();
    }

    @Before
    public void setUp() throws Exception {
        this.et = new EntityTypeImpl<>(className, cls, classIri);
        final Identifier<OWLClassA, URI> id = mock(Identifier.class);
        when(id.getName()).thenReturn(ID_NAME);
        et.setIdentifier(id);
    }

    @Test
    public void hasSingleIdAttributeReturnsAlwaysTrue() {
        assertTrue(et.hasSingleIdAttribute());
    }

    @Test
    public void getAttributesReturnsDeclaredAttributesPlusInheritedAttributes() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        et.getAttributes();
        verify(supertype).getAttributes();
    }

    @Test
    public void getAttributesReturnsDeclaredAttributesWhenThereIsNoSupertype() {
        final AbstractAttribute<OWLClassA, ?> att = mock(AbstractAttribute.class);
        et.addDeclaredAttribute("test", att);
        final Set<Attribute<? super OWLClassA, ?>> result = et.getAttributes();
        assertEquals(1, result.size());
        assertTrue(result.contains(att));
    }

    @Test
    public void getAttributeReturnsInheritedAttribute() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        final String attName = "test";
        final AbstractAttribute att = mock(AbstractAttribute.class);
        doReturn(att).when(supertype).getAttribute(attName);
        assertEquals(att, et.getAttribute(attName));
        verify(supertype).getAttribute(attName);
    }

    @Test
    public void getAttributeThrowsIllegalArgumentWhenAttributeIsNotFoundInDeclaredAndInheritedAttributes() {
        final String attName = "unknownAttribute";
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Attribute " + attName + " is not present in type " + et.toString());
        et.getAttribute(attName);
    }

    @Test
    public void getCollectionThrowsIllegalArgumentWhenElementTypeDoesNotMatch() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Collection attribute " + attName + " with element type " + OWLClassB.class +
                " is not present in type " + et.toString());
        et.getCollection(attName, OWLClassB.class);
    }

    @Test
    public void getListReturnsListAttribute() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final ListAttribute result = et.getList(attName, OWLClassA.class);
        assertEquals(att, result);
    }

    @Test
    public void getSetThrowsIllegalArgumentWhenAttributeIsNotSet() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Set attribute " + attName + " with element type " + OWLClassA.class +
                " is not present in type " + et.toString());
        et.getSet(attName, OWLClassA.class);
    }

    @Test
    public void getPluralAttributesGetsAlsoInheritedPluralAttributes() {
        final CollectionAttribute att = mock(CollectionAttribute.class);
        when(att.isCollection()).thenReturn(true);
        final ListAttributeImpl listAtt = mock(ListAttributeImpl.class);
        when(listAtt.isCollection()).thenReturn(true);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        et.addDeclaredAttribute("test", listAtt);
        when(supertype.getPluralAttributes()).thenReturn(Collections.singleton(att));
        final Set<PluralAttribute<? super OWLClassA, ?, ?>> result = et.getPluralAttributes();
        assertTrue(result.contains(att));
        assertTrue(result.contains(listAtt));
    }

    @Test
    public void getSingularAttributesGetsAlsoInheritedSingularAttributes() {
        final SingularAttribute attOne = mock(SingularAttribute.class);
        when(attOne.isCollection()).thenReturn(false);
        final AbstractAttribute attTwo = mock(AbstractAttribute.class);
        when(attTwo.isCollection()).thenReturn(false);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        et.addDeclaredAttribute("test", attTwo);
        when(supertype.getSingularAttributes()).thenReturn(Collections.singleton(attOne));
        final Set<SingularAttribute<? super OWLClassA, ?>> result = et.getSingularAttributes();
        assertTrue(result.contains(attOne));
        assertTrue(result.contains(attTwo));
    }

    @Test
    public void getSingularAttributeReturnsAttributeWithMatchingNameAndType() {
        final AbstractAttribute attOne = mock(AbstractAttribute.class);
        when(attOne.isCollection()).thenReturn(false);
        final String attName = "test";
        when(attOne.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, attOne);
        final SingularAttribute result = et.getSingularAttribute(attName, OWLClassA.class);
        assertEquals(attOne, result);
    }

    @Test
    public void getSingularAttributeThrowsIllegalArgumentWhenAttributeIsNotSingular() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Singular attribute " + attName + " of type " + OWLClassB.class + " is not present in type " +
                        et.toString());
        et.getSingularAttribute(attName, OWLClassB.class);
    }

    @Test
    public void getDeclaredAttributeReturnsAttributeDeclaredInType() {
        final AbstractAttribute attOne = mock(AbstractAttribute.class);
        when(attOne.isCollection()).thenReturn(false);
        final String attName = "test";
        when(attOne.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, attOne);
        final Attribute result = et.getDeclaredAttribute(attName);
        assertEquals(attOne, result);
    }

    @Test
    public void getDeclaredCollectionReturnsDeclaredCollectionAttribute() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final CollectionAttribute result = et.getDeclaredCollection(attName);
        assertEquals(att, result);
    }

    @Test
    public void getDeclaredListThrowsIllegalArgumentWhenListElementTypeDoesNotMatch() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "List attribute " + attName + " with element type " + OWLClassB.class + " is not declared in type " +
                        et.toString());
        et.getDeclaredList(attName, OWLClassB.class);
    }

    @Test
    public void getDeclaredSetThrowsIllegalArgumentWhenAttributeIsNotSet() {
        final ListAttributeImpl att = mock(ListAttributeImpl.class);
        final String attName = "test";
        when(att.getBindableJavaType()).thenReturn(OWLClassA.class);
        when(att.isCollection()).thenReturn(true);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Set attribute " + attName + " with element type " + OWLClassB.class + " is not declared in type " +
                        et.toString());
        et.getDeclaredSet(attName, OWLClassB.class);
    }

    @Test
    public void getDeclaredSingularAttributeReturnsDeclaredSingularAttribute() {
        final AbstractAttribute att = mock(AbstractAttribute.class);
        final String attName = "test";
        when(att.isCollection()).thenReturn(false);
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        final SingularAttribute result = et.getDeclaredSingularAttribute(attName);
        assertEquals(att, result);
    }

    @Test
    public void getDeclaredSingularAttributeThrowsIllegalArgumentForInvalidType() {
        final AbstractAttribute att = mock(AbstractAttribute.class);
        final String attName = "test";
        when(att.isCollection()).thenReturn(false);
        when(att.getJavaType()).thenReturn(OWLClassA.class);
        et.addDeclaredAttribute(attName, att);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Singular attribute " + attName + " of type " + OWLClassB.class + " is not declared in type " +
                        et.toString());
        et.getDeclaredSingularAttribute(attName, OWLClassB.class);
    }

    @Test
    public void getFieldSpecificationsReturnsAllAttributesAndTypesAndProperties() {
        final SingularAttribute supertypeAtt = mock(SingularAttribute.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
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
    public void getFieldSpecificationsReturnsAlsoIdentifier() {
        final Set<FieldSpecification<? super OWLClassA, ?>> result = et.getFieldSpecifications();
        final Optional<FieldSpecification<? super OWLClassA, ?>> id = result.stream()
                                                                            .filter(fs -> fs.equals(et.getIdentifier()))
                                                                            .findAny();
        assertTrue(id.isPresent());
    }

    @Test
    public void getTypesReturnsTypesAlsoFromSuperType() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        final TypesSpecification types = mock(TypesSpecification.class);
        when(supertype.getTypes()).thenReturn(types);
        final TypesSpecification result = et.getTypes();
        assertEquals(types, result);
    }

    @Test
    public void getPropertiesReturnsPropertiesAlsoFromSuperType() {
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        final PropertiesSpecification properties = mock(PropertiesSpecification.class);
        when(supertype.getProperties()).thenReturn(properties);
        final PropertiesSpecification result = et.getProperties();
        assertEquals(properties, result);
    }

    @Test
    public void getFieldSpecificationGetsFieldSpecification() {
        final SingularAttribute supertypeAtt = mock(SingularAttribute.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        et.setSupertype(supertype);
        final String attName = "test";
        doReturn(supertypeAtt).when(supertype).getFieldSpecification(attName);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(supertypeAtt, result);
    }

    @Test
    public void getFieldSpecificationGetsTypes() {
        final TypesSpecification types = mock(TypesSpecification.class);
        final String attName = "types";
        when(types.getName()).thenReturn(attName);
        et.addDirectTypes(types);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(types, result);
    }

    @Test
    public void getFieldSpecificationGetsDeclaredAttribute() {
        final AbstractPluralAttribute att = mock(AbstractPluralAttribute.class);
        final String attName = "test";
        et.addDeclaredAttribute(attName, att);
        final FieldSpecification<? super OWLClassA, ?> result = et.getFieldSpecification(attName);
        assertEquals(att, result);
    }

    @Test
    public void getFieldSpecificationThrowsIllegalArgumentWhenFieldIsNotFound() {
        final String attName = "unknownAttribute";
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Field " + attName + " is not present in type " + et.toString());
        et.getFieldSpecification(attName);
    }

    @Test
    public void getIdentifierReturnsIdentifierFromSuperclass() {
        final Identifier id = mock(Identifier.class);
        final AbstractIdentifiableType<? super OWLClassA> supertype = spy(new MappedSuperclassTypeImpl<>(Object.class));
        doReturn(id).when(supertype).getIdentifier();
        et.setSupertype(supertype);
        et.setIdentifier(null);
        assertEquals(id, et.getIdentifier());
    }

    @Test
    public void getFieldSpecificationReturnsIdentifier() {
        final FieldSpecification<? super OWLClassA, ?> idSpec = et.getFieldSpecification("uri");
        assertTrue(idSpec instanceof Identifier);
    }
}
