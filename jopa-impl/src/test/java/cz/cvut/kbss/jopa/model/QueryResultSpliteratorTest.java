package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.ontodriver.iteration.ResultRow;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Optional;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@SuppressWarnings("unchecked")
class QueryResultSpliteratorTest {

    @Mock
    private Spliterator<ResultRow> resultSetSpliterator;

    @Mock
    private ResultRow resultRow;

    @Mock
    private Function<ResultRow, Optional<String>> mapper;

    @Mock
    private Procedure closer;

    @Mock
    private Consumer<String> consumer;

    @InjectMocks
    private QueryResultSpliterator<String> sut;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void constructorCreatesSpliteratorWithOrderedNonNullAndImmutableCharacteristics() {
        assertTrue(sut.hasCharacteristics(Spliterator.IMMUTABLE));
        assertTrue(sut.hasCharacteristics(Spliterator.ORDERED));
        assertTrue(sut.hasCharacteristics(Spliterator.NONNULL));
        assertFalse(sut.hasCharacteristics(Spliterator.CONCURRENT));
        assertFalse(sut.hasCharacteristics(Spliterator.DISTINCT));
        assertFalse(sut.hasCharacteristics(Spliterator.SIZED));
        assertFalse(sut.hasCharacteristics(Spliterator.SUBSIZED));
        assertFalse(sut.hasCharacteristics(Spliterator.SORTED));
    }

    @Test
    void tryAdvancePassesResultSetValueToMapperAndThenConsumer() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return true;
        });
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        sut.tryAdvance(consumer);
        verify(mapper).apply(resultRow);
        verify(consumer).accept("test");
    }

    @Test
    void tryAdvanceReturnsTrueWhenResultSetSpliteratorReturnsTrue() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return true;
        });
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        assertTrue(sut.tryAdvance(consumer));
    }

    @Test
    void tryAdvanceReturnsFalseWhenResultSetSpliteratorReturnsFalse() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return false;
        });
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        assertFalse(sut.tryAdvance(consumer));
    }

    @Test
    void tryAdvanceInvokesClosingProcedureWhenResultSetSpliteratorReturnsFalse() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return false;
        });
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        sut.tryAdvance(consumer);
        verify(closer).execute();
    }

    @Test
    void tryAdvanceInvokesClosingProcedureWhenConsumerThrowsException() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return true;
        });
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        doThrow(OWLPersistenceException.class).when(consumer).accept(anyString());
        assertThrows(OWLPersistenceException.class, () -> sut.tryAdvance(consumer));
        verify(closer).execute();
    }

    @Test
    void tryAdvanceDoesNotInvokeConsumerWhenMapperReturnsEmptyOptional() {
        when(resultSetSpliterator.tryAdvance(any())).then(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return true;
        });
        when(mapper.apply(any())).thenReturn(Optional.empty());
        assertTrue(sut.tryAdvance(consumer));
        verify(consumer, never()).accept(any());
    }

    @Test
    void forEachRemainingInvokesMapperAndConsumerForAllRemainingResultSetItems() {
        doAnswer(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return null;
        }).when(resultSetSpliterator).forEachRemaining(any());
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        sut.forEachRemaining(consumer);
        verify(mapper).apply(resultRow);
        verify(consumer).accept("test");
    }

    @Test
    void forEachRemainingInvokesClosingProcedureAfterAllItemsHaveBeenProcessed() {
        doAnswer(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return null;
        }).when(resultSetSpliterator).forEachRemaining(any());
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        sut.forEachRemaining(consumer);
        verify(closer).execute();
    }

    @Test
    void forEachRemainingInvokesClosingProcedureWhenProcessingThrowsException() {
        doAnswer(invocation -> {
            ((Consumer<ResultRow>) invocation.getArgument(0)).accept(resultRow);
            return null;
        }).when(resultSetSpliterator).forEachRemaining(any());
        when(mapper.apply(any())).thenReturn(Optional.of("test"));
        doThrow(OWLPersistenceException.class).when(consumer).accept(anyString());
        assertThrows(OWLPersistenceException.class, () -> sut.forEachRemaining(consumer));
        verify(closer).execute();
    }
}