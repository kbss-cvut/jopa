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
package cz.cvut.kbss.jopa.test;

import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLDataProperty;

import java.io.Serializable;
import java.net.URI;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_Y)
public class OWLClassY implements Serializable {

    @Id(generated = true)
    private URI uri;

    @OWLDataProperty(iri = Vocabulary.P_Y_SINGULAR_MULTILINGUAL_ATTRIBUTE)
    private MultilingualString singularString;

    @OWLDataProperty(iri = Vocabulary.P_Y_PLURAL_MULTILINGUAL_ATTRIBUTE)
    private Set<MultilingualString> pluralString;

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public MultilingualString getSingularString() {
        return singularString;
    }

    public void setSingularString(MultilingualString singularString) {
        this.singularString = singularString;
    }

    public Set<MultilingualString> getPluralString() {
        return pluralString;
    }

    public void setPluralString(Set<MultilingualString> pluralString) {
        this.pluralString = pluralString;
    }

    @Override
    public String toString() {
        return "OWLClassY{" +
                "uri=" + uri +
                ", singularString=" + singularString +
                ", pluralString=" + pluralString +
                '}';
    }
}
