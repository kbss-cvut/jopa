package cz.cvut.kbss.jopa.query;

import cz.cvut.kbss.jopa.query.mapper.ResultRowMapper;
import cz.cvut.kbss.jopa.query.mapper.SparqlResultMapper;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

public class ResultSetMappingManagerTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    private static final String NAME = "testMapping";

    private ResultSetMappingManager manager = new ResultSetMappingManager();

    @Test
    public void addedMapperCanBeRetrieved() {
        final SparqlResultMapper mapper = new ResultRowMapper(NAME);
        manager.addMapper(NAME, mapper);
        final SparqlResultMapper result = manager.getMapper(NAME);
        assertNotNull(result);
        assertEquals(mapper, result);
    }

    @Test
    public void addMapperThrowsIllegalArgumentWhenMapperForSpecifiedNameAlreadyExists() {
        final SparqlResultMapper mapper = new ResultRowMapper(NAME);
        manager.addMapper(NAME, mapper);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Mapping " + NAME + " already exists in this persistence unit.");
        manager.addMapper(NAME, new ResultRowMapper(NAME));
    }

    @Test
    public void getMapperThrowsIllegalArgumentWhenMapperForMappingNameDoesNotExist() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Mapping " + NAME + " not found in this persistence unit.");
        manager.getMapper(NAME);
    }
}