package cz.agents.map4rt.agent;

import cz.agents.map4rt.CommonTime;
import org.jgrapht.DirectedGraph;
import tt.euclid2i.Line;
import tt.euclid2i.Point;
import tt.euclid2i.probleminstance.Environment;

import java.util.Random;

/**
 * A remote version of the COBRA agent, to work on a distributed system
 *
 * @author Martin Rokyta (rokytmar@fel.cvut.cz)
 */
public class RemoteCOBRAAgent extends COBRAAgent {
  /**
   * Flag specifying whether the agent needs a token to start planning a trajectory to new task
   */
  private boolean wantsToStartTask;

  public RemoteCOBRAAgent(String name, Point start, int nTasks, Environment env, DirectedGraph<Point, Line> planningGraph, int agentBodyRadius, float maxSpeed, int maxTime, int timeStep, Random random) {
    super(name, start, nTasks, env, planningGraph, agentBodyRadius, maxSpeed, maxTime, timeStep, random);
    wantsToStartTask = false;
  }

  /**
   * Called each loop to update agent's state
   * @param ms
   */
  public void tick(int ms) {
    if (currentTask != null && !currentTaskTouchedGoal && getCurrentPos().equals(currentTask)) {
      // DESTINATION TOUCHED
      long prolongT = (CommonTime.currentTimeMs() - lastTaskTravelStartedAt) - this.currentTaskBaseDuration;
      prolongTSum += prolongT;
      prolongTSumSq += prolongT * prolongT;

      currentTaskTouchedGoal = true;
    }

    if (currentTask != null && currentTaskDestinationReached()) {
      // DESTINATION REACHED AND THE ROBOT CAN REST
      long prolongR = (CommonTime.currentTimeMs() - lastTaskTravelStartedAt) - this.currentTaskBaseDuration;
      prolongRSum += prolongR;
      prolongRSumSq += prolongR * prolongR;
    }

    // Agent wants to start plannign
    if (currentTask == null && CommonTime.currentTimeMs() > issueFirstTaskAt && nTasks > 0) {
      lastTaskIssuedAt = CommonTime.currentTimeMs();
      wantsToStartTask = true;
    }

    if (currentTask != null && currentTaskDestinationReached()) {
      if (nTasks == 0) {
        LOGGER.info(getName() + " finished all tasks");
        lastTaskReachedAtMs = CommonTime.currentTimeMs();
      }
      currentTask = null;
    }
  }

  /**
   * Called when agent holds a token to plan new trajectory
   */
  public void createNewTask() {
    wantsToStartTask = false;
    long waitDuration = CommonTime.currentTimeMs() - lastTaskIssuedAt;
    waitSum += waitDuration;
    waitSumSq += waitDuration * waitDuration;

    currentTask = CurrentTasks.assignRandomDestination(getName(), random);
    currentTaskBaseDuration = getTaskDuration(getCurrentPos(), currentTask);
    baseSum += currentTaskBaseDuration;
    baseSumSq += currentTaskBaseDuration * currentTaskBaseDuration;

    LOGGER.info(getName() + " Carrying out new task " + currentTask + ", baseline duration is " + currentTaskBaseDuration + ". There is " + nTasks + " tasks in the stack to be carried out.");
    handleNewTask(currentTask);
    nTasks--;
  }

  /**
   * @return true if agent needs a token to start planning
   */
  public boolean wantsToStartTask() {
    return wantsToStartTask;
  }

  /**
   * Gets the amount of task that were not yet planned
   * @return number of tasks yet to be planned
   */
  public int getRemainingTasks() {
    return nTasks;
  }
}
