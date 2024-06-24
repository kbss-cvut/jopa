package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.query.sparql.SparqlConstants;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class ParameterValueTest {

    @Test
    void toQueryValuesReturnsListOfSizeOneWithParameterValueStringifiedForQuery() {
        final ParameterValue sut = new IntegerParameterValue(Generators.randomPositiveInt(1000));
        final List<String> result = sut.toQueryValues(1);
        assertEquals(List.of(sut.getQueryString()), result);
    }

    @Test
    void toQueryValuesReturnsListOfSpecifiedSizeFilledWithValueAndUndef() {
        final ParameterValue sut = new IntegerParameterValue(Generators.randomPositiveInt(1000));
        int size = 5;
        final List<String> result = sut.toQueryValues(size);
        assertEquals(size, result.size());
        assertEquals(sut.getQueryString(), result.get(0));
        result.subList(1, size).forEach(v -> assertEquals(SparqlConstants.UNDEF, v));
    }
}
