/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.lang.reflect.Field;

class TestClass {

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes/stringAtt")
    private String stringAtt;

    @OWLDataProperty(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/attributes/intAtt")
    private Integer intAtt;

    public String getStringAtt() {
        return stringAtt;
    }

    public void setStringAtt(String stringAtt) {
        this.stringAtt = stringAtt;
    }

    public Integer getIntAtt() {
        return intAtt;
    }

    public void setIntAtt(Integer intAtt) {
        this.intAtt = intAtt;
    }

    static Field stringAttField() throws NoSuchFieldException {
        return TestClass.class.getDeclaredField("stringAtt");
    }

    static Field intAttField() throws NoSuchFieldException {
        return TestClass.class.getDeclaredField("intAtt");
    }
}
