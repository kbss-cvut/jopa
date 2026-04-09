package cz.cvut.kbss.jopa.oom.util;

import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.sessions.util.FetchGraphWrapper;

public record ObjectGraphInfo(Descriptor descriptor, FetchGraphWrapper fetchGraph) {

    public ObjectGraphInfo(Descriptor descriptor) {
        this(descriptor, new FetchGraphWrapper());
    }
}
