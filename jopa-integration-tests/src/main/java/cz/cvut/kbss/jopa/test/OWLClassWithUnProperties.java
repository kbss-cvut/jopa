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
package cz.cvut.kbss.jopa.test;


import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.Transient;

import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OwlClassWithUnProperties)
public class OWLClassWithUnProperties implements OWLInterfaceAnMethods {
    @Id
    private URI id;

    @Transient
    private boolean used;
    private String name;

    private Set<String> titles;

    public OWLClassWithUnProperties() {
    }

    public OWLClassWithUnProperties(URI id) {
        this.id = id;
    }

    public URI getId() {
        return id;
    }

    public void setId(URI id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    public boolean getUsed() {
        return used;
    }

    public boolean isUsed() {
        return used;
    }

    @Override
    public Set<String> getTitles() {
        return titles;
    }

    public void setTitles(Set<String> titles) {
        this.titles = titles;
    }
}
