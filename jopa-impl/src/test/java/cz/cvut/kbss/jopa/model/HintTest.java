package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.ontodriver.Statement;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Locale;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
public class HintTest {

    private static final String HINT_NAME = "testHint";

    @Mock
    private AbstractQuery query;

    @Mock
    private Statement statement;

    static class TestHint extends QueryHintsHandler.Hint {

        TestHint(Object defaultValue, Object[][] values) {
            super(HINT_NAME, defaultValue);
            this.valueArray = values;
        }

        @Override
        void applyToQuery(Object hintValue, AbstractQuery query, Statement statement) {

        }
    }

    @Test
    void initializeMapsValueTransformation() {
        final TestHint sut = new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                });
        sut.initialize();
        assertTrue(sut.valueMap.containsKey(Boolean.toString(true).toUpperCase(Locale.ROOT)));
        assertTrue(sut.valueMap.containsKey(Boolean.toString(false).toUpperCase(Locale.ROOT)));
        assertEquals(sut.valueMap.get(Boolean.toString(true).toUpperCase(Locale.ROOT)), Boolean.TRUE);
        assertEquals(sut.valueMap.get(Boolean.toString(false).toUpperCase(Locale.ROOT)), Boolean.FALSE);
        assertNull(sut.valueArray);
        assertNull(sut.defaultValue);
    }

    @Test
    void applyAppliesSpecifiedValueToQuery() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply(Boolean.TRUE, query, statement);
        verify(sut).applyToQuery(Boolean.TRUE, query, statement);
    }

    @Test
    void applyTransformsProvidedValueToCorrespondingMappedValue() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply("false", query, statement);
        verify(sut).applyToQuery(Boolean.FALSE, query, statement);
    }

    @Test
    void applyUsesDefaultValueWhenProvidedValueIsEmptyString() {
        final Boolean defaultValue = Boolean.TRUE;
        final TestHint sut = spy(new TestHint(defaultValue, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        sut.apply("", query, statement);
        verify(sut).applyToQuery(defaultValue, query, statement);
    }

    @Test
    void applyThrowsIllegalArgumentExceptionWhenProvidedValueCannotBeTransformedToExpectedValue() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        sut.initialize();
        assertThrows(IllegalArgumentException.class, () -> sut.apply("not boolean", query, statement));
    }

    @Test
    void applyOnClassFindsCorrectHintInstanceAndAppliesIt() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        QueryHintsHandler.Hint.registerHint(sut);
        QueryHintsHandler.Hint.apply(HINT_NAME, "true", query, statement);
        verify(sut).apply("true", query, statement);
    }

    @Test
    void applyOnClassDoesNothingForUnknownHintName() {
        final TestHint sut = spy(new TestHint(null, new Object[][]{
                {Boolean.TRUE.toString(), Boolean.TRUE},
                {Boolean.FALSE.toString(), Boolean.FALSE},
                }));
        QueryHintsHandler.Hint.registerHint(sut);
        QueryHintsHandler.Hint.apply("unknownHintName", "true", query, statement);
        verify(sut, never()).apply(any(), any(), any());
    }
}
