/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.query.criteria;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaBuilder;
import cz.cvut.kbss.jopa.model.query.criteria.CriteriaQuery;
import cz.cvut.kbss.jopa.model.query.criteria.Order;
import cz.cvut.kbss.jopa.model.query.criteria.Predicate;
import cz.cvut.kbss.jopa.model.query.criteria.Root;
import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CriteriaBuilderTest {

    @Mock
    private UnitOfWork uowMock;

    private static CriteriaBuilder f;

    @BeforeAll
    static void init() {
        f = mock(CriteriaBuilderImpl.class);
    }

    @BeforeEach
    void setUp() throws Exception {
        final MetamodelImpl metamodel = mock(MetamodelImpl.class);
        new MetamodelMocks().setMocks(metamodel);
        final MetamodelProvider mpp = mock(MetamodelProvider.class);
        when(uowMock.getMetamodel()).thenReturn(metamodel);
        when(mpp.getMetamodel()).thenReturn(metamodel);
        when(metamodel.getEntities()).thenReturn(Collections.emptySet());
        when(mpp.isEntityType(any())).thenAnswer(inv -> metamodel.isEntityType(inv.getArgument(0)));

        f = new CriteriaBuilderImpl(metamodel);
    }

    @Test
    public void testCountNullExpressionException() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> f.count(null));
    }

    @Test
    public void testLiteralNullExpressionException() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> f.literal(null));
    }

    @Test
    public void testParameterNamedNullExpressionException() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> f.parameter(null, "name"));
    }

    @Test
    public void testParameterUnnamedNullExpressionException() {
        Assertions.assertThrows(IllegalArgumentException.class, () -> f.parameter(null));
    }

    @Test
    public void testOrderAscending() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final Order order = f.asc(root);
        assertTrue(order.isAscending());
    }

    @Test
    public void testOrderDescending() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        final Order order = f.desc(root);
        assertFalse(order.isAscending());
    }

    @Test
    public void testAndPredicateBooleanOperator() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate predicate = f.and(f.equal(root.getAttr("stringAttribute"), "value"));
        assertEquals(Predicate.BooleanOperator.AND, predicate.getOperator());
    }

    @Test
    public void testOrPredicateBooleanOperator() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate predicate = f.or(f.equal(root.getAttr("stringAttribute"), "value"));
        assertEquals(Predicate.BooleanOperator.OR, predicate.getOperator());
    }

    @Test
    public void testPredicateNegated() {
        CriteriaQuery<OWLClassA> query = f.createQuery(OWLClassA.class);
        Root<OWLClassA> root = query.from(OWLClassA.class);
        Predicate predicate = f.not(f.equal(root.getAttr("stringAttribute"), "value"));
        assertTrue(predicate.isNegated());
    }
}
