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
                .create(OWLClassA.getStrAttField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.simpleLiteral = true;
        pa.language = LANG;
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void hasLanguageReturnsTrueForAttributeWithConfiguredLanguage() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.language = LANG;
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertTrue(sut.hasLanguage());
        assertEquals(LANG, sut.getLanguage());
    }

    @Test
    void hasLanguageReturnsFalseWhenAttributeIsLexicalFormOnly() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.lexicalForm = true;
        pa.language = LANG;
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void hasLanguageReturnsFalseForObjectPropertyAttribute() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassD.getOwlClassAField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassD().entityType(), new NamespaceResolver()));
        pa.persistentAttributeType = Attribute.PersistentAttributeType.OBJECT;
        pa.language = LANG;
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertFalse(sut.hasLanguage());
    }

    @Test
    void getDatatypeReturnsConfiguredAttributeDatatype() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassM.getExplicitDatatypeField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassM().entityType(), new NamespaceResolver()));
        pa.datatype = XSD.DURATION;
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertNotNull(sut.getDatatype());
        assertEquals(XSD.DURATION, sut.getDatatype());
    }

    @Test
    void getDatatypeReturnsNullWhenDefaultValueIsUsedInAnnotation() throws Exception {
        final PropertyAttributes pa = PropertyAttributes
                .create(OWLClassA.getStrAttField(), new FieldMappingValidator(),
                        new TypeBuilderContext<>(metamodelMocks.forOwlClassA().entityType(), new NamespaceResolver()));
        pa.datatype = "";
        final SingularAttribute<?, ?> sut = SingularAttributeImpl.builder(pa).build();
        assertNull(sut.getDatatype());
    }
}
