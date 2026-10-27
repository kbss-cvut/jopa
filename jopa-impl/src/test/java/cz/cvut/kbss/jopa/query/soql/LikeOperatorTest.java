/*
 * JOPA
 * Copyright (C) 2026 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.query.soql;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class LikeOperatorTest {

    private final LikeOperator sut = LikeOperator.like();

    @Test
    void toFilterExpressionTranslatesPercentToRegexAnySequence() {
        assertEquals("REGEX(?param, '.*@example\\\\.com')",
                sut.toFilterExpression("?param", "'%@example.com'"));
    }

    @Test
    void toFilterExpressionTranslatesUnderscoreToRegexAnyChar() {
        assertEquals("REGEX(?param, 'John.')", sut.toFilterExpression("?param", "'John_'"));
    }

    @Test
    void toFilterExpressionTranslatesCombinedWildcards() {
        assertEquals("REGEX(?param, './.*.')", sut.toFilterExpression("?param", "'_/%_'"));
    }

    @Test
    void toFilterExpressionEscapesRegexMetacharacters() {
        assertEquals("REGEX(?param, 'a\\\\.b\\\\(c\\\\)\\\\[d\\\\]')",
                sut.toFilterExpression("?param", "'a.b(c)[d]'"));
    }

    @Test
    void toFilterExpressionTreatsEscapedPercentAsLiteral() {
        assertEquals("REGEX(?param, '100% sure')", sut.toFilterExpression("?param", "'100\\% sure'"));
    }

    @Test
    void toFilterExpressionTreatsEscapedUnderscoreAsLiteral() {
        assertEquals("REGEX(?param, 'snake_case')", sut.toFilterExpression("?param", "'snake\\_case'"));
    }

    @Test
    void toFilterExpressionLeavesVariableUnchanged() {
        assertEquals("REGEX(?param, ?value)", sut.toFilterExpression("?param", "?value"));
    }

    @Test
    void toFilterExpressionLeavesVariableWithUnderscoreUnchanged() {
        assertEquals("REGEX(?param, ?user_name)", sut.toFilterExpression("?param", "?user_name"));
    }

    @Test
    void toFilterExpressionNegatesRegexForNotLike() {
        assertEquals("!REGEX(?param, 'test')",
                LikeOperator.notLike().toFilterExpression("?param", "'test'"));
    }
}
