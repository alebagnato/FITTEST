/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package eu.fittest.fbk.efsm2ct.efsm2mon.tool.model;

import java.util.List;

/**
 * merges a set of transitions into a single transition with a label with is
 * obtained by concatenating labels from original transitions
 *
 * @author tiella
 */
public class SingleTransition {

    private State source;
    private State target;
    private StringBuilder label = new StringBuilder();

    public State getSource() {
        return source;
    }

    public State getTarget() {
        return target;
    }

    public String toString() {
        return source + "->" + target;
    }

    public SingleTransition(State source, State target) {

        this.source = source;
        this.target = target;

    }

    public void addTransition(Transition t) {

        if (source != t.getSource()) {
            throw new IllegalArgumentException("transitions must all have the same source state");
        }

        if (target != t.getTarget()) {
            throw new IllegalArgumentException("transitions must all have the same target state");
        }

        final Mutator mutator = t.getMutator();

        label.append(mutator.getAlias()).append(":").append(mutator.getName()).append("(").append(mutator.getFormalArgs()).append(")").append("\\n");

    }

    public String getLabel() {
        return label.toString();
    }

}
