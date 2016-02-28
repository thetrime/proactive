package org.proactive.prolog;

import gnu.prolog.term.Term;
import gnu.prolog.vm.TermConstants;

public class PrologState
{
    private Term t;
    private static PrologState emptyState = new PrologState(TermConstants.emptyListAtom);
    public PrologState(Term t)
    {
        // DEBUG: Check that this is a list, I guess?
        this.t = t;
    }
    public Term getValue()
    {
        return t;
    }

    public static PrologState emptyState()
    {
        return emptyState;
    }

}
