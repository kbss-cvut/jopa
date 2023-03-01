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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.utils.NamespaceResolver;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class AbstractAttributeTest {

    private static final String LANG = "en";

    private MetamodelMocks metamodelMocks;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    void hasLanguageReturnsFalseWhenAttributeIsSimpleLiteral() throws Exception {

        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.simpleLiteral = true;
        pa.language = LANG;
        pa.iri = metamodelMocks.forOwlClassA().stringAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void hasLanguageReturnsTrueForAttributeWithConfiguredLanguage() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.language = LANG;
        pa.iri = metamodelMocks.forOwlClassA().stringAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertTrue(sut.hasLanguage());
        assertEquals(LANG, sut.getLanguage());
    }

    @Test
    void hasLanguageReturnsFalseWhenAttributeIsLexicalFormOnly() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.lexicalForm = true;
        pa.language = LANG;
        pa.iri = metamodelMocks.forOwlClassA().stringAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void hasLanguageReturnsFalseForObjectPropertyAttribute() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassD.getOwlClassAFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassD().entityType(), new NamespaceResolver()));
        pa.persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        pa.language = LANG;
        pa.iri = metamodelMocks.forOwlClassA().stringAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void getDatatypeReturnsConfiguredAttributeDatatype() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassM.getExplicitDatatypeFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassM().entityType(), new NamespaceResolver()));
        pa.datatype = XSD.DURATION;
        pa.iri = metamodelMocks.forOwlClassM().explicitDatatypeAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertNotNull(sut.getDatatype());
        assertEquals(XSD.DURATION, sut.getDatatype());
    }

    @Test
    void getDatatypeReturnsNullWhenDefaultValueIsUsedInAnnotation() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttFieldPropertyInfo(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.datatype = "";
        pa.iri = metamodelMocks.forOwlClassA().stringAttribute().getIRI();
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertNull(sut.getDatatype());
    }
}
