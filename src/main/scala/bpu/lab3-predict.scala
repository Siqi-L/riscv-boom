package boom

import Chisel._
import freechips.rocketchip.config.{Parameters, Field}

case class Lab3Parameters(
  enabled: Boolean = true,
  history_length: Int = 1,
  info_size: Int = 7,
  num_entries: Int = 512)

case object Lab3Key extends Field[Lab3Parameters]

class Lab3BrPredictor(
    fetch_width: Int = 1,
    history_length: Int = 1,
    info_size: Int = 7,
  	num_entries: Int = 128)(implicit p: Parameters)
      extends BrPredictor(fetch_width, history_length)(p)
{
  /* not predicting. Change this for your predictor */
  // io.resp.valid := false.B

  // Predictor state: as in Alpha 21264’s “tournament” branch predictor
  val local_history_table = Reg(
    init = Vec(Seq.fill(num_entries) { UInt("b00", width = 9) }))

  val local_prediction = Reg(
    init = Vec(Seq.fill(math.pow(2,9).toInt) { UInt("b00", width = 3) }))

  val global_reg = Reg(init = Vec(Seq.fill(1) { UInt("b00", width = 10) }))

  val global_prediction = Reg(
    init = Vec(Seq.fill(math.pow(2,10).toInt) { UInt("b00", width = 2) }))

  val choice_prediction = Reg(
    init = Vec(Seq.fill(math.pow(2,10).toInt) { UInt("b00", width = 2) }))

  // pause prediction
  val stall = !io.resp.ready

  // index into local prediction by first index into get local history
  val s1_pc = io.req_pc
  val s1_r_idx = s1_pc >> (UInt(log2Ceil(coreInstBytes))-2.U)
  val s1_local_idx = RegEnable(local_history_table(s1_r_idx), !stall)
  val s1_local_count = RegEnable(local_prediction(s1_local_idx), !stall)

  // index into global and choice prediction
  val s1_global_count = RegEnable(global_prediction(global_reg(0)), !stall)
  val s1_choice_count = RegEnable(choice_prediction(global_reg(0)), !stall)


  // keep sending predictions as long as not disabled
  io.resp.valid := !this.disable_bpd
  // if choice upper bit is 0 then prediction is the upper bit of local counter
  // otherwise prediction is the upper bit of global counter
  when(s1_choice_count(1)) {
	io.resp.bits.takens := s1_local_count(2)
  } 
  when(~s1_choice_count(1)) {
  	io.resp.bits.takens := s1_global_count(1)
  }
  	
  // tell the pipeline to save the index for commit
  io.resp.bits.info := RegNext(s1_r_idx)


  // on commit, get the index and whether the branch was actually taken
  val commit_s1_en = this.commit.valid
  val commit_s1_r_idx = this.commit.bits.info.info
  val commit_s1_taken = this.commit.bits.ctrl.taken(0)

  // index into table to get previous state
  val commit_s2_r_idx = RegEnable(commit_s1_r_idx, commit_s1_en)
  val commit_s2_global_reg = RegEnable(global_reg(0), commit_s1_en)
  val commit_s2_local_idx = RegEnable(local_history_table(commit_s1_r_idx), commit_s1_en)
  val commit_s2_local_count = RegEnable(local_prediction(commit_s2_local_idx), commit_s1_en)
  val commit_s2_global_count = RegEnable(global_prediction(commit_s2_global_reg), commit_s1_en)
  val commit_s2_choice_count = RegEnable(choice_prediction(commit_s2_global_reg), commit_s1_en)
  val commit_s2_taken = RegEnable(commit_s1_taken, commit_s1_en)
  val commit_s2_en = RegNext(commit_s1_en)
  val predict = Reg(init = Vec(Seq.fill(1) { UInt("b0", width = 1) }))
  when(commit_s1_en & commit_s2_choice_count(0)) { 
        predict(0) := commit_s2_local_count(2)
  } 
  when(~commit_s1_en | ~commit_s2_choice_count(0)){
	predict(0) := commit_s2_global_count(1)
  } 

  // calculate updated index and counter values
  val commit_s2_local_idx_update = Mux(commit_s2_taken, 
    commit_s2_local_idx << 1.U + 1.U, commit_s2_local_idx << 1.U)

  val commit_s2_local_count_update = Mux(commit_s2_taken,
    Mux(commit_s2_local_count === "b111".U, commit_s2_local_count, commit_s2_local_count + 1.U),
    Mux(commit_s2_local_count === "b000".U, commit_s2_local_count, commit_s2_local_count - 1.U))

  val commit_s2_global_reg_update = Mux(commit_s2_taken,
    commit_s2_global_reg << 1.U + 1.U, commit_s2_global_reg << 1.U)

  val commit_s2_global_count_update = Mux(commit_s2_taken,
    Mux(commit_s2_global_count === "b11".U, commit_s2_global_count, commit_s2_global_count + 1.U),
    Mux(commit_s2_global_count === "b00".U, commit_s2_global_count, commit_s2_global_count - 1.U))

  val commit_s2_choice_count_update =  Mux(~commit_s2_taken^(predict(0) === "b0".U),
    Mux (commit_s2_choice_count(1), 
	Mux(commit_s2_choice_count === "b11".U, commit_s2_choice_count, commit_s2_choice_count + 1.U),
	Mux(commit_s2_choice_count === "b00".U, commit_s2_choice_count, commit_s2_choice_count - 1.U)),
    Mux (commit_s2_choice_count(1), commit_s2_choice_count - 1.U, commit_s2_choice_count + 1.U))

  // write back to table
  when (commit_s2_en) { 
  	local_history_table(commit_s2_r_idx) := commit_s2_local_idx_update
  	local_prediction(commit_s2_local_idx) := commit_s2_local_count_update
  	global_prediction(commit_s2_global_reg) := commit_s2_global_count_update
  	choice_prediction(commit_s2_global_reg) := commit_s2_choice_count_update
  	global_reg(0) := commit_s2_global_reg_update
  }

}
