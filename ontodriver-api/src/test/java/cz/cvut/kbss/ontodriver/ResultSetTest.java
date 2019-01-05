package cz.cvut.kbss.ontodriver;

import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import cz.cvut.kbss.ontodriver.iteration.ResultSetIterator;
import org.junit.jupiter.api.Test;

import java.util.Iterator;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ResultSetTest {

    @Test
    void iteratorCreatesResultSetIteratorOverResultSetInstance() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.iterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(true);
        final Iterator<ResultRow> result = sut.iterator();
        assertNotNull(result);
        assertTrue(result instanceof ResultSetIterator);
    }

    @Test
    void iteratorThrowsIllegalStateWhenResultSetIsClosed() {
        final ResultSet sut = mock(ResultSet.class);
        when(sut.iterator()).thenCallRealMethod();
        when(sut.isOpen()).thenReturn(false);
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> sut.iterator());
        assertThat(result.getMessage(), containsString("result set is closed"));
    }
}