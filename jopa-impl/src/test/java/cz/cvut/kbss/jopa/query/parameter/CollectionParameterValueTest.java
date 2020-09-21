package cz.cvut.kbss.jopa.query.parameter;

import cz.cvut.kbss.jopa.environment.utils.Generators;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CollectionParameterValueTest {

    private static final BooleanParameterValue V_ONE = new BooleanParameterValue(false);
    private static final IntegerParameterValue V_TWO = new IntegerParameterValue(117);
    private static final UriParameterValue V_THREE = new UriParameterValue(Generators.createIndividualIdentifier());

    @Test
    void getValueReturnsValuesOfIndividualElements() {
        final CollectionParameterValue sut = new CollectionParameterValue(Arrays.asList(V_ONE, V_TWO, V_THREE));

        assertEquals(Arrays.asList(V_ONE.getValue(), V_TWO.getValue(), V_THREE.getValue()), sut.getValue());
    }

    @Test
    void getQueryStringReturnsQueryStringRepresentationsOfIndividualElementsJoinedByComma() {
        final CollectionParameterValue sut = new CollectionParameterValue(Arrays.asList(V_ONE, V_TWO, V_THREE));

        assertEquals(
                Stream.of(V_ONE, V_TWO, V_THREE).map(ParameterValue::getQueryString).collect(Collectors.joining(",")),
                sut.getQueryString());
    }
}
