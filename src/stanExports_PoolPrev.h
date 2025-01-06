// Generated by rstantools.  Do not edit by hand.

/*
    PoolTestR is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    PoolTestR is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with PoolTestR.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.32.2
#include <stan/model/model_header.hpp>
namespace model_PoolPrev_namespace {
using stan::model::model_base_crtp;
using namespace stan::math;
stan::math::profile_map profiles__;
static constexpr std::array<const char*, 32> locations_array__ =
  {" (found before start of program)",
  " (in 'string', line 17, column 2 to column 27)",
  " (in 'string', line 20, column 2 to column 36)",
  " (in 'string', line 21, column 2 to column 9)",
  " (in 'string', line 22, column 2 to column 10)",
  " (in 'string', line 24, column 2 to column 36)",
  " (in 'string', line 36, column 4 to column 35)",
  " (in 'string', line 35, column 7 to line 37, column 3)",
  " (in 'string', line 28, column 4 to column 11)",
  " (in 'string', line 29, column 4 to column 10)",
  " (in 'string', line 31, column 8 to column 30)",
  " (in 'string', line 32, column 8 to column 52)",
  " (in 'string', line 30, column 17 to line 33, column 5)",
  " (in 'string', line 30, column 4 to line 33, column 5)",
  " (in 'string', line 34, column 4 to column 23)",
  " (in 'string', line 27, column 19 to line 35, column 3)",
  " (in 'string', line 27, column 2 to line 37, column 3)",
  " (in 'string', line 38, column 2 to column 35)",
  " (in 'string', line 2, column 2 to column 17)",
  " (in 'string', line 3, column 8 to column 9)",
  " (in 'string', line 3, column 2 to column 40)",
  " (in 'string', line 4, column 18 to column 19)",
  " (in 'string', line 4, column 2 to column 30)",
  " (in 'string', line 5, column 2 to column 27)",
  " (in 'string', line 6, column 2 to column 26)",
  " (in 'string', line 7, column 2 to column 38)",
  " (in 'string', line 10, column 8 to column 9)",
  " (in 'string', line 10, column 2 to column 47)",
  " (in 'string', line 13, column 4 to column 37)",
  " (in 'string', line 11, column 15 to line 14, column 3)",
  " (in 'string', line 11, column 2 to line 14, column 3)",
  " (in 'string', line 20, column 27 to column 28)"};
#include <stan_meta_header.hpp>
class model_PoolPrev final : public model_base_crtp<model_PoolPrev> {
private:
  int N;
  std::vector<int> Result;
  Eigen::Matrix<double,-1,1> PoolSize_data__;
  double PriorAlpha;
  double PriorBeta;
  int JeffreysPrior;
  std::vector<int> FlippedResult;
  Eigen::Map<Eigen::Matrix<double,-1,1>> PoolSize{nullptr, 0};
public:
  ~model_PoolPrev() {}
  model_PoolPrev(stan::io::var_context& context__, unsigned int
                 random_seed__ = 0, std::ostream* pstream__ = nullptr)
      : model_base_crtp(0) {
    int current_statement__ = 0;
    using local_scalar_t__ = double;
    boost::ecuyer1988 base_rng__ =
      stan::services::util::create_rng(random_seed__, 0);
    // suppress unused var warning
    (void) base_rng__;
    static constexpr const char* function__ =
      "model_PoolPrev_namespace::model_PoolPrev";
    // suppress unused var warning
    (void) function__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      current_statement__ = 18;
      context__.validate_dims("data initialization", "N", "int",
        std::vector<size_t>{});
      N = std::numeric_limits<int>::min();
      current_statement__ = 18;
      N = context__.vals_i("N")[(1 - 1)];
      current_statement__ = 18;
      stan::math::check_greater_or_equal(function__, "N", N, 1);
      current_statement__ = 19;
      stan::math::validate_non_negative_index("Result", "N", N);
      current_statement__ = 20;
      context__.validate_dims("data initialization", "Result", "int",
        std::vector<size_t>{static_cast<size_t>(N)});
      Result = std::vector<int>(N, std::numeric_limits<int>::min());
      current_statement__ = 20;
      Result = context__.vals_i("Result");
      current_statement__ = 20;
      stan::math::check_greater_or_equal(function__, "Result", Result, 0);
      current_statement__ = 20;
      stan::math::check_less_or_equal(function__, "Result", Result, 1);
      current_statement__ = 21;
      stan::math::validate_non_negative_index("PoolSize", "N", N);
      current_statement__ = 22;
      context__.validate_dims("data initialization", "PoolSize", "double",
        std::vector<size_t>{static_cast<size_t>(N)});
      PoolSize_data__ = Eigen::Matrix<double,-1,1>::Constant(N,
                          std::numeric_limits<double>::quiet_NaN());
      new (&PoolSize)
        Eigen::Map<Eigen::Matrix<double,-1,1>>(PoolSize_data__.data(), N);
      {
        std::vector<local_scalar_t__> PoolSize_flat__;
        current_statement__ = 22;
        PoolSize_flat__ = context__.vals_r("PoolSize");
        current_statement__ = 22;
        pos__ = 1;
        current_statement__ = 22;
        for (int sym1__ = 1; sym1__ <= N; ++sym1__) {
          current_statement__ = 22;
          stan::model::assign(PoolSize, PoolSize_flat__[(pos__ - 1)],
            "assigning variable PoolSize", stan::model::index_uni(sym1__));
          current_statement__ = 22;
          pos__ = (pos__ + 1);
        }
      }
      current_statement__ = 22;
      stan::math::check_greater_or_equal(function__, "PoolSize", PoolSize, 0);
      current_statement__ = 23;
      context__.validate_dims("data initialization", "PriorAlpha", "double",
        std::vector<size_t>{});
      PriorAlpha = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 23;
      PriorAlpha = context__.vals_r("PriorAlpha")[(1 - 1)];
      current_statement__ = 23;
      stan::math::check_greater_or_equal(function__, "PriorAlpha",
        PriorAlpha, 0);
      current_statement__ = 24;
      context__.validate_dims("data initialization", "PriorBeta", "double",
        std::vector<size_t>{});
      PriorBeta = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 24;
      PriorBeta = context__.vals_r("PriorBeta")[(1 - 1)];
      current_statement__ = 24;
      stan::math::check_greater_or_equal(function__, "PriorBeta", PriorBeta,
        0);
      current_statement__ = 25;
      context__.validate_dims("data initialization", "JeffreysPrior", "int",
        std::vector<size_t>{});
      JeffreysPrior = std::numeric_limits<int>::min();
      current_statement__ = 25;
      JeffreysPrior = context__.vals_i("JeffreysPrior")[(1 - 1)];
      current_statement__ = 25;
      stan::math::check_greater_or_equal(function__, "JeffreysPrior",
        JeffreysPrior, 0);
      current_statement__ = 25;
      stan::math::check_less_or_equal(function__, "JeffreysPrior",
        JeffreysPrior, 1);
      current_statement__ = 26;
      stan::math::validate_non_negative_index("FlippedResult", "N", N);
      current_statement__ = 27;
      FlippedResult = std::vector<int>(N, std::numeric_limits<int>::min());
      current_statement__ = 30;
      for (int n = 1; n <= N; ++n) {
        current_statement__ = 28;
        stan::model::assign(FlippedResult, (1 -
          stan::model::rvalue(Result, "Result", stan::model::index_uni(n))),
          "assigning variable FlippedResult", stan::model::index_uni(n));
      }
      current_statement__ = 27;
      stan::math::check_greater_or_equal(function__, "FlippedResult",
        FlippedResult, 0);
      current_statement__ = 27;
      stan::math::check_less_or_equal(function__, "FlippedResult",
        FlippedResult, 1);
      current_statement__ = 31;
      stan::math::validate_non_negative_index("qpool", "N", N);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    num_params_r__ = 1;
  }
  inline std::string model_name() const final {
    return "model_PoolPrev";
  }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.32.2",
             "stancflags = --allow-undefined"};
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI,
            stan::require_vector_like_t<VecR>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR>
  log_prob_impl(VecR& params_r__, VecI& params_i__, std::ostream*
                pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    static constexpr const char* function__ =
      "model_PoolPrev_namespace::log_prob";
    // suppress unused var warning
    (void) function__;
    try {
      local_scalar_t__ p = DUMMY_VAR__;
      current_statement__ = 1;
      p = in__.template read_constrain_lub<local_scalar_t__, jacobian__>(0,
            1, lp__);
      Eigen::Matrix<local_scalar_t__,-1,1> qpool =
        Eigen::Matrix<local_scalar_t__,-1,1>::Constant(N, DUMMY_VAR__);
      local_scalar_t__ q = DUMMY_VAR__;
      current_statement__ = 4;
      q = (1 - p);
      current_statement__ = 5;
      stan::model::assign(qpool,
        stan::math::exp(
          stan::math::elt_multiply(stan::math::log1m(p), PoolSize)),
        "assigning variable qpool");
      current_statement__ = 2;
      stan::math::check_greater_or_equal(function__, "qpool", qpool, 0);
      current_statement__ = 2;
      stan::math::check_less_or_equal(function__, "qpool", qpool, 1);
      {
        current_statement__ = 16;
        if (JeffreysPrior) {
          local_scalar_t__ s = DUMMY_VAR__;
          current_statement__ = 9;
          s = 0;
          current_statement__ = 13;
          for (int n = 1; n <= N; ++n) {
            local_scalar_t__ PS = DUMMY_VAR__;
            current_statement__ = 10;
            PS = stan::model::rvalue(PoolSize, "PoolSize",
                   stan::model::index_uni(n));
            current_statement__ = 11;
            s = (s + ((stan::math::pow(PS, 2.0) *
              stan::math::pow(q, (PS - 2))) / (1 - stan::math::pow(q, PS))));
          }
          current_statement__ = 14;
          lp_accum__.add((stan::math::log(s) / 2));
        } else {
          current_statement__ = 6;
          lp_accum__.add(stan::math::beta_lpdf<propto__>(p, PriorAlpha,
                           PriorBeta));
        }
        current_statement__ = 17;
        lp_accum__.add(stan::math::bernoulli_lpmf<propto__>(FlippedResult,
                         qpool));
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
  }
  template <typename RNG, typename VecR, typename VecI, typename VecVar,
            stan::require_vector_like_vt<std::is_floating_point,
            VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral,
            VecI>* = nullptr, stan::require_vector_vt<std::is_floating_point,
            VecVar>* = nullptr>
  inline void
  write_array_impl(RNG& base_rng__, VecR& params_r__, VecI& params_i__,
                   VecVar& vars__, const bool
                   emit_transformed_parameters__ = true, const bool
                   emit_generated_quantities__ = true, std::ostream*
                   pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    static constexpr bool propto__ = true;
    // suppress unused var warning
    (void) propto__;
    double lp__ = 0.0;
    // suppress unused var warning
    (void) lp__;
    int current_statement__ = 0;
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    constexpr bool jacobian__ = false;
    static constexpr const char* function__ =
      "model_PoolPrev_namespace::write_array";
    // suppress unused var warning
    (void) function__;
    try {
      double p = std::numeric_limits<double>::quiet_NaN();
      current_statement__ = 1;
      p = in__.template read_constrain_lub<local_scalar_t__, jacobian__>(0,
            1, lp__);
      Eigen::Matrix<double,-1,1> qpool =
        Eigen::Matrix<double,-1,1>::Constant(N,
          std::numeric_limits<double>::quiet_NaN());
      double q = std::numeric_limits<double>::quiet_NaN();
      out__.write(p);
      if (stan::math::logical_negation(
            (stan::math::primitive_value(emit_transformed_parameters__) ||
            stan::math::primitive_value(emit_generated_quantities__)))) {
        return ;
      }
      current_statement__ = 4;
      q = (1 - p);
      current_statement__ = 5;
      stan::model::assign(qpool,
        stan::math::exp(
          stan::math::elt_multiply(stan::math::log1m(p), PoolSize)),
        "assigning variable qpool");
      current_statement__ = 2;
      stan::math::check_greater_or_equal(function__, "qpool", qpool, 0);
      current_statement__ = 2;
      stan::math::check_less_or_equal(function__, "qpool", qpool, 1);
      if (emit_transformed_parameters__) {
        out__.write(qpool);
        out__.write(q);
      }
      if (stan::math::logical_negation(emit_generated_quantities__)) {
        return ;
      }
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, typename VecI,
            stan::require_vector_t<VecVar>* = nullptr,
            stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void
  unconstrain_array_impl(const VecVar& params_r__, const VecI& params_i__,
                         VecVar& vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::deserializer<local_scalar_t__> in__(params_r__, params_i__);
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ p = DUMMY_VAR__;
      current_statement__ = 1;
      p = in__.read<local_scalar_t__>();
      out__.write_free_lub(0, 1, p);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  template <typename VecVar, stan::require_vector_t<VecVar>* = nullptr>
  inline void
  transform_inits_impl(const stan::io::var_context& context__, VecVar&
                       vars__, std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    stan::io::serializer<local_scalar_t__> out__(vars__);
    int current_statement__ = 0;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    // suppress unused var warning
    (void) DUMMY_VAR__;
    try {
      current_statement__ = 1;
      context__.validate_dims("parameter initialization", "p", "double",
        std::vector<size_t>{});
      int pos__ = std::numeric_limits<int>::min();
      pos__ = 1;
      local_scalar_t__ p = DUMMY_VAR__;
      current_statement__ = 1;
      p = context__.vals_r("p")[(1 - 1)];
      out__.write_free_lub(0, 1, p);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
    }
  }
  inline void
  get_param_names(std::vector<std::string>& names__, const bool
                  emit_transformed_parameters__ = true, const bool
                  emit_generated_quantities__ = true) const {
    names__ = std::vector<std::string>{"p"};
    if (emit_transformed_parameters__) {
      std::vector<std::string> temp{"qpool", "q"};
      names__.reserve(names__.size() + temp.size());
      names__.insert(names__.end(), temp.begin(), temp.end());
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  get_dims(std::vector<std::vector<size_t>>& dimss__, const bool
           emit_transformed_parameters__ = true, const bool
           emit_generated_quantities__ = true) const {
    dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{}};
    if (emit_transformed_parameters__) {
      std::vector<std::vector<size_t>>
        temp{std::vector<size_t>{static_cast<size_t>(N)},
             std::vector<size_t>{}};
      dimss__.reserve(dimss__.size() + temp.size());
      dimss__.insert(dimss__.end(), temp.begin(), temp.end());
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  constrained_param_names(std::vector<std::string>& param_names__, bool
                          emit_transformed_parameters__ = true, bool
                          emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "p");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= N; ++sym1__) {
        param_names__.emplace_back(std::string() + "qpool" + '.' +
          std::to_string(sym1__));
      }
      param_names__.emplace_back(std::string() + "q");
    }
    if (emit_generated_quantities__) {}
  }
  inline void
  unconstrained_param_names(std::vector<std::string>& param_names__, bool
                            emit_transformed_parameters__ = true, bool
                            emit_generated_quantities__ = true) const final {
    param_names__.emplace_back(std::string() + "p");
    if (emit_transformed_parameters__) {
      for (int sym1__ = 1; sym1__ <= N; ++sym1__) {
        param_names__.emplace_back(std::string() + "qpool" + '.' +
          std::to_string(sym1__));
      }
      param_names__.emplace_back(std::string() + "q");
    }
    if (emit_generated_quantities__) {}
  }
  inline std::string get_constrained_sizedtypes() const {
    return std::string("[{\"name\":\"p\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"qpool\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(N) + "},\"block\":\"transformed_parameters\"},{\"name\":\"q\",\"type\":{\"name\":\"real\"},\"block\":\"transformed_parameters\"}]");
  }
  inline std::string get_unconstrained_sizedtypes() const {
    return std::string("[{\"name\":\"p\",\"type\":{\"name\":\"real\"},\"block\":\"parameters\"},{\"name\":\"qpool\",\"type\":{\"name\":\"vector\",\"length\":" + std::to_string(N) + "},\"block\":\"transformed_parameters\"},{\"name\":\"q\",\"type\":{\"name\":\"real\"},\"block\":\"transformed_parameters\"}]");
  }
  // Begin method overload boilerplate
  template <typename RNG> inline void
  write_array(RNG& base_rng, Eigen::Matrix<double,-1,1>& params_r,
              Eigen::Matrix<double,-1,1>& vars, const bool
              emit_transformed_parameters = true, const bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = 1;
    const size_t num_transformed = emit_transformed_parameters * ((N + 1));
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    std::vector<int> params_i;
    vars = Eigen::Matrix<double,-1,1>::Constant(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <typename RNG> inline void
  write_array(RNG& base_rng, std::vector<double>& params_r, std::vector<int>&
              params_i, std::vector<double>& vars, bool
              emit_transformed_parameters = true, bool
              emit_generated_quantities = true, std::ostream*
              pstream = nullptr) const {
    const size_t num_params__ = 1;
    const size_t num_transformed = emit_transformed_parameters * ((N + 1));
    const size_t num_gen_quantities = emit_generated_quantities * (0);
    const size_t num_to_write = num_params__ + num_transformed +
      num_gen_quantities;
    vars = std::vector<double>(num_to_write,
             std::numeric_limits<double>::quiet_NaN());
    write_array_impl(base_rng, params_r, params_i, vars,
      emit_transformed_parameters, emit_generated_quantities, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(Eigen::Matrix<T_,-1,1>& params_r, std::ostream* pstream = nullptr) const {
    Eigen::Matrix<int,-1,1> params_i;
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  template <bool propto__, bool jacobian__, typename T_> inline T_
  log_prob(std::vector<T_>& params_r, std::vector<int>& params_i,
           std::ostream* pstream = nullptr) const {
    return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
  }
  inline void
  transform_inits(const stan::io::var_context& context,
                  Eigen::Matrix<double,-1,1>& params_r, std::ostream*
                  pstream = nullptr) const final {
    std::vector<double> params_r_vec(params_r.size());
    std::vector<int> params_i;
    transform_inits(context, params_i, params_r_vec, pstream);
    params_r = Eigen::Map<Eigen::Matrix<double,-1,1>>(params_r_vec.data(),
                 params_r_vec.size());
  }
  inline void
  transform_inits(const stan::io::var_context& context, std::vector<int>&
                  params_i, std::vector<double>& vars, std::ostream*
                  pstream__ = nullptr) const {
    vars.resize(num_params_r__);
    transform_inits_impl(context, vars, pstream__);
  }
  inline void
  unconstrain_array(const std::vector<double>& params_constrained,
                    std::vector<double>& params_unconstrained, std::ostream*
                    pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = std::vector<double>(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
  inline void
  unconstrain_array(const Eigen::Matrix<double,-1,1>& params_constrained,
                    Eigen::Matrix<double,-1,1>& params_unconstrained,
                    std::ostream* pstream = nullptr) const {
    const std::vector<int> params_i;
    params_unconstrained = Eigen::Matrix<double,-1,1>::Constant(num_params_r__,
                             std::numeric_limits<double>::quiet_NaN());
    unconstrain_array_impl(params_constrained, params_i,
      params_unconstrained, pstream);
  }
};
}
using stan_model = model_PoolPrev_namespace::model_PoolPrev;
#ifndef USING_R
// Boilerplate
stan::model::model_base&
new_model(stan::io::var_context& data_context, unsigned int seed,
          std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_PoolPrev_namespace::profiles__;
}
#endif
#endif
