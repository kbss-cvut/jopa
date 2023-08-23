/**
 * Copyright (C) 2023 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;

import java.net.URI;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Set;

@OWLClass(iri = Vocabulary.C_OWL_CLASS_PART_CONSTR_IN_PARENT)
public class OWLClassWithAnnotatedMethodsInInterfaceParent implements OWLInterfaceE {

    @Id
    private URI uri;

    protected OWLClassWithUnProperties data;
    protected Set<OWLClassWithUnProperties> dataList;
    protected ZoneOffset withConverter;
    protected Color ordinalEnumAttribute;
    protected List<URI> simpleList;

    public OWLClassWithAnnotatedMethodsInInterfaceParent() {
    }

    public OWLClassWithAnnotatedMethodsInInterfaceParent(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    @Override
    public OWLClassWithUnProperties getData() {
        return data;
    }

    public void setData(OWLClassWithUnProperties data) {
        this.data = data;
    }

    @Override
    public Set<OWLClassWithUnProperties> getDataList() {
        return dataList;
    }

    public void setDataList(Set<OWLClassWithUnProperties> dataList) {
        this.dataList = dataList;
    }

    @Override
    public ZoneOffset getWithConverter() {
        return withConverter;
    }

    public void setWithConverter(ZoneOffset withConverter) {
        this.withConverter = withConverter;
    }

    @Override
    public Color getOrdinalEnumAttribute() {
        return ordinalEnumAttribute;
    }

    public void setOrdinalEnumAttribute(Color ordinalEnumAttribute) {
        this.ordinalEnumAttribute = ordinalEnumAttribute;
    }

    @Override
    public List<URI> getSimpleList() {
        return simpleList;
    }

    public void setSimpleList(List<URI> simpleList) {
        this.simpleList = simpleList;
    }
}
