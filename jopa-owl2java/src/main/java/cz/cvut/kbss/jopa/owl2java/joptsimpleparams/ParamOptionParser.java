package cz.cvut.kbss.jopa.owl2java.joptsimpleparams;

import joptsimple.OptionParser;
import joptsimple.OptionSpecBuilder;

/**
 * Option parser capable of accepting our params
 */
public class ParamOptionParser extends OptionParser {
    public OptionSpecBuilder accepts(final Param p) {
        return accepts(p.arg, p.description);
    }
}
