/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.soql;

public class AttributeNode extends SoqlNode {

    private String value;
    private String iri = "";

    public AttributeNode(String value) {
        this.value = value;
    }

    public AttributeNode(SoqlNode parent, String value) {
        super(parent);
        this.value = value;
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public String getCapitalizedValue() {
        assert value != null;
        return value.substring(0, 1).toUpperCase() + value.substring(1);
    }

    @Override
    public void setValue(String value) {
        this.value = value;
    }

    @Override
    public String getIri() {
        return iri;
    }

    @Override
    public void setIri(String iri) {
        this.iri = iri;
    }

    @Override
    public boolean requiresFilterExpression() {
        return false;
    }

    @Override
    public String toFilterExpression(String filterParam, String filterValue) {
        return filterParam;
    }
}
