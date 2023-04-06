/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.query.soql;

public class SoqlNode {

    private SoqlNode parent;
    private SoqlNode child;
    private String value;
    private String iri = "";

    public SoqlNode(String value) {
        this.value = value;
    }

    public SoqlNode(SoqlNode parent, String value) {
        this.parent = parent;
        this.value = value;
    }

    public SoqlNode(SoqlNode other) {
        this.parent = other.parent;
        this.child = other.child;
        this.value = other.value;
        this.iri = other.iri;
    }

    public boolean hasNextChild() {
        return this.child != null;
    }

    public SoqlNode getChild() {
        return this.child;
    }

    public boolean hasNextParent() {
        return this.parent != null;
    }

    public SoqlNode getParent() {
        return this.parent;
    }

    public String getValue() {
        return this.value;
    }

    public String getCapitalizedValue() {
        return this.value.substring(0, 1).toUpperCase() + this.value.substring(1);
    }

    public void setChild(SoqlNode child) {
        this.child = child;
    }

    public void setParent(SoqlNode parent) {
        this.parent = parent;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getIri() {
        return iri;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }


}
