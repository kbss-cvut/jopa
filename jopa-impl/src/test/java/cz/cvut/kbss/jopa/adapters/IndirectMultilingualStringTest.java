package cz.cvut.kbss.jopa.adapters;

import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class IndirectMultilingualStringTest {

    private MultilingualString referencedString;
    private Map<String, String> translations;
    private OWLClassU owner;
    private Field field;

    @Mock
    private UnitOfWorkImpl uow;

    private IndirectMultilingualString sut;

    @BeforeEach
    void setUp() throws Exception {
        this.translations = new HashMap<>();
        translations.put("en", "building");
        translations.put("cs", "stavba");
        this.referencedString = new MultilingualString(translations);
        this.owner = new OWLClassU(Generators.createIndividualIdentifier());
        this.field = OWLClassU.getSingularStringAttField();
        this.sut = new IndirectMultilingualString(owner, field, uow, referencedString);
    }

    @Test
    void constructorThrowsNullPointerForNullReferencedString() {
        assertThrows(NullPointerException.class,
                () -> new IndirectMultilingualString(owner, field, uow, null));
    }

    @Test
    void constructorThrowsNullPointerForNullUnitOfWork() {
        assertThrows(NullPointerException.class,
                () -> new IndirectMultilingualString(owner, field, null, referencedString));
    }

    @Test
    void setWithValueAndLanguagePropagatesChangeAndNotifiesPersistenceContext() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        sut.set("de", "der Bau");
        verify(uow).attributeChanged(owner, field);
        assertEquals(translations.size() + 1, referencedString.getValue().size());
        assertTrue(referencedString.contains("de"));
        assertEquals("der Bau", referencedString.get("de"));
    }

    @Test
    void setWithValuePropagatesChangeAndNotifiesPersistenceContext() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        sut.set("der Bau");
        verify(uow).attributeChanged(owner, field);
        assertEquals(translations.size() + 1, referencedString.getValue().size());
        assertEquals("der Bau", referencedString.get());
    }

    @Test
    void removePropagatesChangeAndNotifiesPersistenceContext() {
        when(uow.isInTransaction()).thenReturn(Boolean.TRUE);
        sut.remove("cs");
        verify(uow).attributeChanged(owner, field);
        assertEquals(translations.size() - 1, referencedString.getValue().size());
        assertFalse(referencedString.contains("cs"));
    }

    @Test
    void readOperationsPropagateToReferencedInstance() {
        assertEquals(referencedString.get(), sut.get());
        assertEquals(referencedString.get("cs"), sut.get("cs"));
        assertEquals(referencedString.getValue(), sut.getValue());
        assertEquals(referencedString.getLanguages(), sut.getLanguages());
        assertEquals(referencedString.containsSimple(), sut.containsSimple());
        assertEquals(referencedString.contains("de"), sut.contains("de"));
        assertEquals(referencedString.contains("en"), sut.contains("en"));
        assertEquals(referencedString.isEmpty(), sut.isEmpty());
    }

    @Test
    void equalsWorksWithUnwrappedMultilingualString() {
        assertEquals(sut, referencedString);
        assertEquals(sut, new IndirectMultilingualString(owner, field, uow, referencedString));
        final MultilingualString another = new MultilingualString(translations);
        another.set("de", "der Bau");
        assertNotEquals(sut, another);
        assertNotEquals(sut, new IndirectMultilingualString(owner, field, uow, another));
        assertNotEquals(sut, "test");
    }

    @Test
    void hashCodeWorksWithUnwrappedMultilingualString() {
        assertEquals(referencedString.hashCode(), sut.hashCode());
    }

    @Test
    void getReferencedStringReturnsWrappedInstance() {
        assertSame(referencedString, sut.unwrap());
    }

    @Test
    void toStringInvokesToStringOfReferencedInstance() {
        assertEquals(referencedString.toString(), sut.toString());
    }

    @Test
    void equalsWorksFromNormalToIndirectMultilingualString() {
        assertEquals(referencedString, sut);
    }
}
